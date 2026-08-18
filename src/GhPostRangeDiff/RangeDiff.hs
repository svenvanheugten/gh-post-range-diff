-- | Run `git range-diff` and parse its output into values, so rendering
-- doesn't have to work with the text.
module GhPostRangeDiff.RangeDiff (Change (..), Commit (..), rangeDiff) where

import GhPostRangeDiff.Git qualified as Git

-- | What happened to one commit between the old and the new range.
data Change
  = Added
  | Removed
  | Unchanged
  | -- | Same commit, different diff. Carries the interdiff git printed
    -- underneath the header, de-indented so its own +/- sit in column 0.
    Updated [String]
  deriving (Eq, Show)

-- | One commit, as `git range-diff` reports it.
data Commit = Commit
  { cmChange :: Change,
    -- | The side that still exists: the new sha for =/!/>, the old one for <.
    cmCommitSha :: String,
    cmCommitMessage :: String
  }
  deriving (Eq, Show)

-- | A range-diff line is a header iff it doesn't start with whitespace; the
-- interdiff body git emits under a @!@ entry is always indented.
isHeader :: String -> Bool
isHeader (c : _) = c /= ' '
isHeader _ = False

-- | Pair each header with the body lines beneath it. Body lines before the
-- first header (which shouldn't happen) are dropped.
chunks :: [String] -> [(String, [String])]
chunks ls = case dropWhile (not . isHeader) ls of
  [] -> []
  (h : rest) -> let (body, more) = break isHeader rest in (h, body) : chunks more

-- | Drop up to @n@ leading spaces, undoing the indent git gives the interdiff.
stripIndent :: Int -> String -> String
stripIndent n (' ' : s) | n > 0 = stripIndent (n - 1) s
stripIndent _ s = s

-- | Read one commit from its header line, e.g.
--
-- > 1:  5ed838c ! 1:  f69a2d3 second commit
--
-- The marker is one of @=@ (unchanged), @!@ (same commit, different diff),
-- @>@ (added), @<@ (removed). For an added commit the old side is @-  -------@;
-- for a removed one the new side is.
mkCommit :: String -> [String] -> Commit
mkCommit l body = case words l of
  (_ : oldSha : [m] : _ : newSha : subj) -> case m of
    '>' -> Commit Added newSha (unwords subj)
    -- A removed commit only exists on the old side; everything else has a
    -- new-side sha we can link to.
    '<' -> Commit Removed oldSha (unwords subj)
    '=' -> Commit Unchanged newSha (unwords subj)
    '!' -> Commit (Updated (map (stripIndent 4) body)) newSha (unwords subj)
    _ -> bad
  _ -> bad
  where
    bad = error ("unexpected range-diff header: " ++ l)

-- | Parse `git range-diff` output, one 'Commit' per header line.
parse :: String -> [Commit]
parse = map (uncurry mkCommit) . chunks . lines

-- | Line up @oldBase..oldHead@ against @newBase..newHead@: one 'Commit' per
-- commit in either range, saying what happened to it.
rangeDiff :: String -> String -> String -> String -> IO [Commit]
rangeDiff oldBase oldHead newBase newHead =
  parse <$> Git.rangeDiff oldBase oldHead newBase newHead
