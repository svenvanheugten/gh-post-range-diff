-- | Run `git range-diff` and parse its output into values, so rendering
-- doesn't have to work with the text.
module GhPostRangeDiff.RangeDiff
  ( Change (..),
    Commit (..),
    CommitSha (shaText),
    commitSha,
    Interdiff (interdiffText),
    interdiff,
    rangeDiff,
  )
where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import GhPostRangeDiff.Git qualified as Git

-- | An abbreviated commit sha, as `git range-diff` prints it.
newtype CommitSha = CommitSha {shaText :: String}
  deriving (Eq, Show)

-- | Read a sha, rejecting anything that isn't one. Git abbreviates to at least
-- four hex digits and never past the full forty, and prints them in lowercase.
commitSha :: String -> Maybe CommitSha
commitSha s
  | n >= 4, n <= 40, all isHex s = Just (CommitSha s)
  | otherwise = Nothing
  where
    n = length s
    isHex c = isDigit c || c `elem` ['a' .. 'f']

-- | The interdiff git printed under a @!@ entry, de-indented so its own +/-
-- sit in column 0. Always ends with a newline; see 'interdiff'.
newtype Interdiff = Interdiff {interdiffText :: String}
  deriving (Eq, Show)

-- | Read an interdiff, adding a newline if it doesn't end with one.
interdiff :: String -> Interdiff
interdiff = Interdiff . unlines . lines

-- | What happened to one commit between the old and the new range.
data Change
  = Added
  | Removed
  | Unchanged
  | -- | Same commit, different diff. Carries the interdiff git printed
    -- underneath the header.
    Updated Interdiff
  deriving (Eq, Show)

-- | One commit, as `git range-diff` reports it.
data Commit = Commit
  { cmChange :: Change,
    -- | The side that still exists: the new sha for =/!/>, the old one for <.
    cmCommitSha :: CommitSha,
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
    '>' -> commit Added newSha subj
    -- A removed commit only exists on the old side; everything else has a
    -- new-side sha we can link to.
    '<' -> commit Removed oldSha subj
    '=' -> commit Unchanged newSha subj
    '!' -> commit (Updated (interdiff (unlines (map (stripIndent 4) body)))) newSha subj
    _ -> bad
  _ -> bad
  where
    -- The side we didn't pick is `-------`, so only the one we keep is read.
    commit change sha subj = Commit change (fromMaybe bad (commitSha sha)) (unwords subj)
    bad = error ("unexpected range-diff header: " ++ l)

-- | Parse `git range-diff` output, one 'Commit' per header line.
parse :: String -> [Commit]
parse = map (uncurry mkCommit) . chunks . lines

-- | Line up @oldBase..oldHead@ against @newBase..newHead@: one 'Commit' per
-- commit in either range, saying what happened to it.
rangeDiff :: String -> String -> String -> String -> IO [Commit]
rangeDiff oldBase oldHead newBase newHead =
  parse <$> Git.rangeDiff oldBase oldHead newBase newHead
