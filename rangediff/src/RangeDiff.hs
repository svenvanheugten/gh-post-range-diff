-- | The range-diff between two versions of a branch, behind a handle, so that
-- where it comes from is somebody else's business.
module RangeDiff
  ( Change (..),
    Commit (..),
    CommitMessage (messageText),
    commitMessage,
    Interdiff (interdiffShape, interdiffText),
    Shape (..),
    interdiff,
    interdiffLines,
    Handle (..),
  )
where

import Data.List (isPrefixOf)
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
data Interdiff = Interdiff
  { -- | What it is a diff of, which says how its lines are laid out.
    interdiffShape :: Shape,
    interdiffText :: String
  }
  deriving (Eq, Show)

-- | What an interdiff is a diff of. Either way it is a diff, so each line of
-- its body opens with diff markers -- but not as many of them.
data Shape
  = -- | of the two commits' patches, as `git range-diff` prints it: a diff of
    -- two diffs, so a line of one opens with two markers, what the interdiff
    -- does to it and then what the commit does to it.
    DiffOfPatches
  | -- | of the two commits themselves, as `jj interdiff` prints it: a diff in
    -- git's own format, so a line of one opens with the one marker.
    DiffOfCommits
  deriving (Eq, Show)

-- | Read an interdiff, adding a newline if it doesn't end with one.
interdiff :: Shape -> String -> Interdiff
interdiff shape = Interdiff shape . unlines . lines

-- | Each line of an interdiff, split after the diff markers it opens with: the
-- columns that say what the line does in the diff, rather than what it holds.
-- A line that isn't a line of a diff's body -- a header naming a file, or a
-- hunk -- opens with none.
interdiffLines :: Interdiff -> [(String, String)]
interdiffLines (Interdiff shape text) = case shape of
  DiffOfPatches -> map patchesLine (lines text)
  DiffOfCommits -> commitsLines (lines text)

-- | What a diff marks a line with: kept, added or removed.
isMarker :: Char -> Bool
isMarker c = c `elem` " +-"

-- | Split a line of a 'DiffOfPatches' after its markers: the spaces, `+` and
-- `-` it opens with, up to two.
--
-- `git range-diff` shows how a commit's patch changed by diffing the old patch
-- against the new one, so a line of the patch opens with two markers: the
-- outer diff's, then the patch's own. Here a rewrite changed a comment in
-- Widget.hs from 30 seconds to 5. On the right is what each line is split
-- after:
--
-- > @@ Commit message                ""
-- >   ## Widget.hs ##                "  "
-- >  @@                              " "
-- >   module Widget where            "  "
-- > -+-- Gives up after 30 seconds.  "-+"
-- > ++-- Gives up after 5 seconds.   "++"
--
-- * The first line is the outer diff's hunk header, and has no markers.
-- * The third is the patch's hunk header, and has only the outer diff's.
-- * The second is a file header. It has a space where the patch's marker
--   would go, and counting that space as a marker does no harm.
-- * In the last two, the `--` after the markers is the start of the text, a
--   Haskell comment. That is why it stops at two.
--
-- Going by the look of a line only works because git leaves out the patch's
-- `---` and `+++` lines, which open with what looks like a marker but isn't.
-- 'commitsLines' handles a diff that has them.
patchesLine :: String -> (String, String)
patchesLine l = splitAt (length (take 2 (takeWhile isMarker l))) l

-- | Split each line of a 'DiffOfCommits' after its marker.
--
-- `jj interdiff --git` prints a plain git diff, where a line in a hunk opens
-- with one marker and a header line with none. Here is the rewrite from
-- 'patchesLine', along with a change it made to README.md. On the right is
-- what each line is split after:
--
-- > diff --git a/README.md b/README.md     ""
-- > index 1f09111946..6304bab339 100644    ""
-- > --- a/README.md                        ""
-- > +++ b/README.md                        ""
-- > @@ -1,1 +1,1 @@                        ""
-- > -Widgets give up after 30 seconds.     "-"
-- > +Widgets give up after 5 seconds.      "+"
-- > \ No newline at end of file            ""
-- > diff --git a/Widget.hs b/Widget.hs     ""
-- > index 95ba4c612d..7d48bb233c 100644    ""
-- > --- a/Widget.hs                        ""
-- > +++ b/Widget.hs                        ""
-- > @@ -1,2 +1,2 @@                        ""
-- >  module Widget where                   " "
-- > --- Gives up after 30 seconds.         "-"
-- > +-- Gives up after 5 seconds.          "+"
--
-- Unlike in 'patchesLine', the look of a line doesn't tell which kind it is.
-- The header line `--- a/Widget.hs` looks like a removed line, and the
-- removed line `--- Gives up after 30 seconds.` looks like a header line.
--
-- So this goes through the lines in order, keeping track of whether it is in
-- a header or a hunk:
--
-- * A `diff` line starts a header, which runs up to the next hunk header.
-- * A hunk header starts a hunk, which runs up to the next hunk header or
--   `diff` line. Above, the second `diff` line ends the README.md hunk, so
--   the `---` line after it is a header line again.
--
-- In a hunk, a line opening with a space, `+` or `-` has that one marker.
-- Every other line has none: the headers, and the "No newline at end of file"
-- note.
commitsLines :: [String] -> [(String, String)]
commitsLines = go False
  where
    go _ [] = []
    go inHunk (l : ls)
      | "diff " `isPrefixOf` l = ("", l) : go False ls
      | "@@" `isPrefixOf` l = ("", l) : go True ls
      | inHunk, (c : rest) <- l, isMarker c = ([c], rest) : go True ls
      | otherwise = ("", l) : go inHunk ls

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
