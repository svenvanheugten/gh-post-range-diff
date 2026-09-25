-- | Turn a parsed range-diff into a Markdown list for a GitHub PR comment.
--
-- Each commit becomes a list item with a status emoji, an auto-linked commit
-- sha, and a subject. A changed commit's item is a collapsed spoiler, titled by
-- that same line, holding its interdiff as a fenced diff block.
module GhPostRangeDiff.Render (format) where

import Data.List (group, intercalate)
import RangeDiff (Change (..), Commit (..), interdiffText, messageText)
import RangeDiff.CommitSha (shaText)

-- | The longest run of consecutive backticks in a string. Used to size a code
-- fence so it can't be closed early by backticks in the content.
maxBacktickRun :: String -> Int
maxBacktickRun s = maximum (0 : [length g | g@('`' : _) <- group s])

-- | The commit's one-line summary: status, sha and subject. The status word is
-- emphasised by the caller, since how to do that depends on where the line
-- lands — see 'markdownStrong' and 'htmlStrong'.
headline :: (String -> String) -> Commit -> String
headline strong (Commit change sha subj) =
  -- The sha is left bare (not in a `code` span) so GitHub auto-links it to
  -- the commit.
  tag ++ " " ++ shaText sha ++ " " ++ messageText subj
  where
    tag = case change of
      Added -> "\128994 " ++ strong "Added" -- 🟢
      Removed -> "\128308 " ++ strong "Removed" -- 🔴
      Updated _ -> "\128992 " ++ strong "Updated" -- 🟠
      Unchanged -> "\9898 " ++ strong "Unchanged" -- ⚪

-- | Emphasis for a headline that stands on its own, as Markdown.
markdownStrong :: String -> String
markdownStrong s = "**" ++ s ++ "**"

-- | Emphasis for a headline inside a <summary>. It has to be a tag: the
-- <details> element opens a raw-HTML block, in which Markdown emphasis would
-- come out as literal asterisks.
htmlStrong :: String -> String
htmlStrong s = "<strong>" ++ s ++ "</strong>"

render :: Commit -> String
render commit@(Commit change _ _) = case change of
  -- Only a changed commit carries an interdiff worth showing, and only a
  -- non-empty one is worth a spoiler.
  Updated patch | patch' <- interdiffText patch, not (null patch') -> spoiler commit patch'
  _ -> headline markdownStrong commit

-- | Hide a patch behind a spoiler that the commit's own headline titles, so
-- that a long interdiff doesn't bury the commits below it.
--
-- The blank lines inside the element are needed: without them GitHub renders
-- the fence as literal text instead of as Markdown.
spoiler :: Commit -> String -> String
spoiler commit patch =
  "<details>\n<summary>"
    ++ headline htmlStrong commit
    ++ "</summary>\n\n"
    ++ fenced patch
    ++ "\n\n</details>"

-- | Show a patch as a fenced diff block. The fence is longer than any backtick
-- run in the patch, so a line like ``` inside the interdiff can't close the
-- block early. The closing fence needs no newline in front of it: a
-- 'RangeDiff.Interdiff' ends on a line boundary.
fenced :: String -> String
fenced patch = fence ++ "diff\n" ++ patch ++ fence
  where
    fence = replicate (max 3 (maxBacktickRun patch + 1)) '`'

-- | Render the commits as a Markdown list, one item per commit.
format :: [Commit] -> String
format = intercalate "\n\n" . map render
