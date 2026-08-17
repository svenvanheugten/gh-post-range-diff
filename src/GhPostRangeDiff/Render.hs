{- | Turn `git range-diff` output into a Markdown list for a GitHub PR comment.

The raw range-diff is a flat block of text. This renders it as a per-commit
list with a status emoji, an auto-linked commit sha, and the interdiff of a
changed commit shown in-place as a fenced diff block.
-}
module GhPostRangeDiff.Render (format) where

import Data.List (group, intercalate, unsnoc)

{- | The longest run of consecutive backticks in a string. Used to size a code
fence so it can't be closed early by backticks in the content.
-}
maxBacktickRun :: String -> Int
maxBacktickRun s = maximum (0 : [length g | g@('`' : _) <- group s])

{- | One commit line from `git range-diff`, e.g.

> 1:  5ed838c ! 1:  f69a2d3 second commit

The marker is one of @=@ (unchanged), @!@ (same commit, different diff),
@>@ (added), @<@ (removed). For an added commit the old side is @-  -------@;
for a removed one the new side is. 'enBody' holds the indented interdiff lines
git emits underneath a @!@ entry.
-}
data Entry = Entry {_enMarker :: Char, _enSha, _enSubject :: String, enBody :: [String]}

{- | A range-diff line is a header iff it doesn't start with whitespace; the
interdiff body git emits under a @!@ entry is always indented.
-}
isHeader :: String -> Bool
isHeader (c : _) = c /= ' '
isHeader _ = False

mkEntry :: String -> Entry
mkEntry l = case words l of
    (_ : oldSha : [m] : _ : newSha : subj) ->
        -- A removed commit only exists on the old side; everything else has a
        -- new-side sha we can link to.
        Entry m (if m == '<' then oldSha else newSha) (unwords subj) []
    _ -> error ("unexpected range-diff header: " ++ l)

{- | Split range-diff output into entries, attaching each indented body line to
the entry above it.
-}
entries :: [String] -> [Entry]
entries = foldl step []
  where
    step acc l
        | isHeader l = acc ++ [mkEntry l]
        | otherwise = case unsnoc acc of
            Just (pre, e) -> pre ++ [e{enBody = enBody e ++ [l]}]
            Nothing -> acc -- body before any header: shouldn't happen

{- | Drop up to @n@ leading spaces, so the interdiff's own +/- land in column 0
and render as a diff.
-}
stripIndent :: Int -> String -> String
stripIndent n (' ' : s) | n > 0 = stripIndent (n - 1) s
stripIndent _ s = s

render :: Entry -> String
render (Entry m sha subj body) =
    -- The sha is left bare (not in a `code` span) so GitHub auto-links it to
    -- the commit.
    tag ++ " " ++ sha ++ " " ++ subj ++ details
  where
    tag = case m of
        '>' -> "\128994 **Added**" -- 🟢
        '<' -> "\128308 **Removed**" -- 🔴
        '!' -> "\128992 **Updated**" -- 🟠
        '=' -> "\9898 **Unchanged**" -- ⚪
        _ -> "\10068 **?**" -- ❔
        -- Only a @!@ entry carries an interdiff worth showing.
    details
        | m == '!' && not (null body) =
            "\n\n" ++ fence ++ "diff\n" ++ unlines stripped ++ fence
        | otherwise = ""
      where
        stripped = map (stripIndent 4) body
        -- Use a fence longer than any backtick run in the patch, so a line
        -- like ``` inside the interdiff can't close the block early.
        fence = replicate (max 3 (maxBacktickRun (unlines stripped) + 1)) '`'

-- | Render `git range-diff` output as a Markdown list, one item per commit.
format :: String -> String
format diff = intercalate "\n\n" (map render (entries (lines diff)))
