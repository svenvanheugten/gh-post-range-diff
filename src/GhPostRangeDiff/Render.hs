-- | Turn a parsed range-diff into a Markdown list for a GitHub PR comment.
--
-- Each commit becomes a list item with a status emoji, an auto-linked commit
-- sha, and — for a changed commit — its interdiff shown in-place as a fenced
-- diff block.
module GhPostRangeDiff.Render (format) where

import Data.List (group, intercalate, isSuffixOf)
import GhPostRangeDiff.RangeDiff (Change (..), Commit (..), Interdiff (..), shaText)

-- | The longest run of consecutive backticks in a string. Used to size a code
-- fence so it can't be closed early by backticks in the content.
maxBacktickRun :: String -> Int
maxBacktickRun s = maximum (0 : [length g | g@('`' : _) <- group s])

render :: Commit -> String
render (Commit change sha subj) =
  -- The sha is left bare (not in a `code` span) so GitHub auto-links it to
  -- the commit.
  tag ++ " " ++ shaText sha ++ " " ++ subj ++ details
  where
    tag = case change of
      Added -> "\128994 **Added**" -- 🟢
      Removed -> "\128308 **Removed**" -- 🔴
      Updated _ -> "\128992 **Updated**" -- 🟠
      Unchanged -> "\9898 **Unchanged**" -- ⚪
      -- Only a changed commit carries an interdiff worth showing.
    details = case change of
      Updated (Interdiff patch) -> fenced patch
      _ -> ""

-- | Show a patch as a fenced diff block. The fence is longer than any backtick
-- run in the patch, so a line like ``` inside the interdiff can't close the
-- block early. An empty patch gets no block at all.
fenced :: String -> String
fenced "" = ""
fenced patch = "\n\n" ++ fence ++ "diff\n" ++ terminated patch ++ fence
  where
    fence = replicate (max 3 (maxBacktickRun patch + 1)) '`'

-- | A patch with a trailing newline, so the closing fence starts on a line of
-- its own rather than running on from the last line of the diff.
terminated :: String -> String
terminated s
  | "\n" `isSuffixOf` s = s
  | otherwise = s ++ "\n"

-- | Render the commits as a Markdown list, one item per commit.
format :: [Commit] -> String
format = intercalate "\n\n" . map render
