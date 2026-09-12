-- | The range-diff between two versions of a branch, behind a handle, so that
-- it can be mocked. `git range-diff` prints it as text; everything here reads
-- that text into values, so that whatever reports on it doesn't have to work
-- with the text.
module RangeDiff
  ( Change (..),
    Commit (..),
    CommitMessage (messageText),
    commitMessage,
    Interdiff (interdiffText),
    interdiff,
    Handle (..),
    git,
  )
where

import Data.Bifunctor (first)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import RangeDiff.CommitSha (CommitSha, commitSha, shaText)
import System.Process (readProcess)

-- | The subject line of a commit, as `git range-diff` prints it. Holds no
-- newline of its own; see 'commitMessage'.
newtype CommitMessage = CommitMessage {messageText :: String}
  deriving (Eq, Show)

-- | Read a commit message, keeping only the first line. git stores a whole
-- message but range-diff prints the subject alone, on the header line, so that
-- is the only part there is to read here.
commitMessage :: String -> CommitMessage
commitMessage = CommitMessage . takeWhile (/= '\n')

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
    cmCommitMessage :: CommitMessage
  }
  deriving (Eq, Show)

-- | A range-diff line is a header iff it is indented by fewer than four
-- spaces. git indents the interdiff body by exactly four, and right-aligns the
-- commit numbers in a header to the width of the largest one — so once either
-- range reaches ten commits, a single-digit header carries a leading space of
-- its own. Only a range of ten thousand commits could pad one out to four.
-- A blank line is never a header; it can only have come from the interdiff.
isHeader :: String -> Bool
isHeader l = case span (== ' ') l of
  (_, "") -> False
  (indent, _) -> length indent < 4

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

-- | Split a line into its first @n@ space-separated fields and whatever
-- follows them. That remainder loses only the single space that separated it
-- from field @n@.
peel :: Int -> String -> Maybe ([String], String)
peel 0 s = Just ([], s)
peel n s = case span (/= ' ') (dropWhile (== ' ') s) of
  ("", _) -> Nothing
  (f, rest) -> first (f :) <$> peel (n - 1) (drop 1 rest)

-- | Read one commit from its header line, e.g.
--
-- > 1:  5ed838c ! 1:  f69a2d3 second commit
--
-- The marker is one of @=@ (unchanged), @!@ (same commit, different diff),
-- @>@ (added), @<@ (removed). For an added commit the old side is @-  -------@;
-- for a removed one the new side is. Everything past the fifth field is the
-- commit message, taken verbatim -- except that git takes it from the old side
-- of a pair, so a @!@ commit that was reworded is read under the message its
-- interdiff says it was reworded to. See 'rewordedTo'.
mkCommit :: String -> [String] -> Commit
mkCommit l body = case peel 5 l of
  Just ([_, oldSha, [m], _, newSha], subj) -> case m of
    '>' -> commit Added newSha subj
    -- A removed commit only exists on the old side; everything else has a
    -- new-side sha we can link to.
    '<' -> commit Removed oldSha subj
    '=' -> commit Unchanged newSha subj
    '!' -> commit (Updated (interdiff (unlines hunks))) newSha (fromMaybe subj (rewordedTo hunks))
    _ -> bad
  _ -> bad
  where
    hunks = map (stripIndent 4) body
    -- The side we didn't pick is `-------`, so only the one we keep is read.
    commit change sha subj = Commit change (fromMaybe bad (commitSha sha)) (commitMessage subj)
    bad = error ("unexpected range-diff header: " ++ l)

-- | The subject a commit now goes under, if its interdiff shows it.
--
-- git diffs each commit's metadata, then its message, then its patch, with the
-- message under a line of its own and the subject straight below that:
--
-- > @@ Metadata
-- >  Author: t <t@t>
-- >
-- >   ## Commit message ##
-- > -    Add the widget
-- > +    Add a widget
--
-- A reworded subject always brings that line along as context, so where it is
-- missing the subject did not change and the header has it right. Only the
-- first hunk is looked in, and only if it starts in the metadata: a patch could
-- hold a line that reads the same, but never above the message.
rewordedTo :: [String] -> Maybe String
rewordedTo ("@@ Metadata" : hunk) = drop 5 <$> find onNewSide message
  where
    message = drop 1 (dropWhile (/= "  ## Commit message ##") (takeWhile (not . isPrefixOf "@@") hunk))
    -- Past the old subject, if there is one, to the new one; then past the
    -- interdiff's own marker and the indent git gives a message.
    onNewSide s = take 1 s `elem` [" ", "+"]
rewordedTo _ = Nothing

-- | Parse `git range-diff` output, one 'Commit' per header line.
parse :: String -> [Commit]
parse = map (uncurry mkCommit) . chunks . lines

-- | Everything there is to ask of a range-diff.
newtype Handle = Handle
  { -- | Line up @oldBase..oldHead@ against @newBase..newHead@: one 'Commit'
    -- per commit in either range, saying what happened to it.
    rangeDiff :: CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO [Commit]
  }

-- | Range-diffs taken in the repo in @dir@, as the `git` CLI takes them.
git :: FilePath -> Handle
git dir = Handle {rangeDiff = gitRangeDiff dir}

-- Read what `git range-diff` prints for the two ranges. Colour is asked off
-- outright, since a repo or user configured to have it always would otherwise
-- wrap every header in escape codes. Context is asked for outright too, since
-- one configured to have none would leave a reworded subject without the line
-- that says it is the subject; see 'rewordedTo'.
gitRangeDiff :: FilePath -> CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO [Commit]
gitRangeDiff dir oldBase oldHead newBase newHead = parse <$> readProcess "git" args ""
  where
    args = ["-C", dir, "range-diff", "--no-color", "--unified=3", range oldBase oldHead, range newBase newHead]
    range a b = shaText a ++ ".." ++ shaText b
