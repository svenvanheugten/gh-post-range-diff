-- | The range-diff between two versions of a branch, behind a handle, so that
-- where it comes from is somebody else's business.
--
-- Nothing here takes a range-diff. This is only what one says, read into
-- values, so that whatever reports on it doesn't have to work with the text
-- some tool printed. "RangeDiff.Git" reads `git range-diff`'s text;
-- "RangeDiff.Jj" works one out with jj; "RangeDiff.Auto" picks between them.
module RangeDiff
  ( Change (..),
    Commit (..),
    CommitMessage (messageText),
    commitMessage,
    Interdiff (interdiffText),
    interdiff,
    Handle (..),
  )
where

import RangeDiff.CommitSha (CommitSha)

-- | The subject line of a commit, as a range-diff reports it. Holds no newline
-- of its own; see 'commitMessage'.
newtype CommitMessage = CommitMessage {messageText :: String}
  deriving (Eq, Show)

-- | Read a commit message, keeping only the first line. A commit holds a whole
-- message but a range-diff names it by its subject alone, so that is the only
-- part there is to read here.
commitMessage :: String -> CommitMessage
commitMessage = CommitMessage . takeWhile (/= '\n')

-- | The interdiff shown under a commit a rewrite changed: the diff of what that
-- commit did before against what it does now. Always ends with a newline; see
-- 'interdiff'.
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
  | -- | Same commit, different diff. Carries the interdiff from the one to the
    -- other.
    Updated Interdiff
  deriving (Eq, Show)

-- | One commit, as a range-diff reports it.
data Commit = Commit
  { cmChange :: Change,
    -- | The side that still exists: the new sha for a commit the new range has,
    -- the old one for a commit only the old range had.
    cmCommitSha :: CommitSha,
    cmCommitMessage :: CommitMessage
  }
  deriving (Eq, Show)

-- | Everything there is to ask of a range-diff.
newtype Handle = Handle
  { -- | Line up @oldBase..oldHead@ against @newBase..newHead@: one 'Commit'
    -- per commit in either range, saying what happened to it.
    rangeDiff :: CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO [Commit]
  }
