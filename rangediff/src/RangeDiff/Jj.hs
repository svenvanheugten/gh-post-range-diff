-- | The range-diff between two versions of a branch, worked out with jj.
--
-- jj is told a good deal less than `git range-diff` is, so more of the work is
-- here — but it is told the one thing that matters, so the work is easier.
-- Every commit jj makes carries a change id that survives every rewrite of it,
-- so which commit of the old range is which commit of the new one is not
-- something to guess at: it is written on the commits. That makes this the
-- better of the two where it can be used, which is what "RangeDiff.Auto"
-- decides.
--
-- What is left is to have a repo jj can be asked about, to walk the two ranges
-- together, and to ask `jj interdiff` what a rewrite did to each commit both
-- of them hold.
module RangeDiff.Jj
  ( jj,
    colocated,
  )
where

import Control.Monad (unless, void)
import Data.List (find, isPrefixOf)
import Data.List.Extra (trim)
import Data.Maybe (isNothing)
import RangeDiff (Change (..), Commit (..), CommitMessage, Handle (..), commitMessage, interdiff)
import RangeDiff.CommitSha (CommitSha, knownSha, shaText)
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode, readProcessWithExitCode)

-- | jj's own name for a commit, which survives every rewrite of it: a reword,
-- an amend, a rebase, and the rebase any of those sets off above it. Two
-- commits are two versions of the same commit exactly when this matches.
newtype ChangeId = ChangeId String
  deriving (Eq)

-- | One commit of one range, as jj lists it.
data Entry = Entry
  { enChangeId :: ChangeId,
    enCommitSha :: CommitSha,
    enCommitMessage :: CommitMessage
  }

-- | Range-diffs taken in the repo in @dir@, worked out with the `jj` CLI.
--
-- Only good for a repo whose commits carry change ids; see "RangeDiff.Auto",
-- which is what should be picking this. The repo itself needs to be nothing
-- more than a repo git can read: one jj hasn't been set up in is set up
-- here. See 'colocated'.
jj :: FilePath -> Handle
jj dir = Handle {rangeDiff = jjRangeDiff dir}

jjRangeDiff :: FilePath -> CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO [Commit]
jjRangeDiff dir oldBase oldHead newBase newHead = do
  showJj dir [oldBase, oldHead, newBase, newHead]
  was <- commitsIn dir oldBase oldHead
  now <- commitsIn dir newBase newHead
  mapM (report dir) (lineUp was now)

-- * Lining the two ranges up

-- | What became of one commit, before anyone has asked what a rewrite did to
-- it.
data Fate
  = -- | only the old range has it
    Dropped Entry
  | -- | only the new range has it
    PutIn Entry
  | -- | both do, under the same change id: the old side, then the new one
    Kept Entry Entry

-- | Walk the two ranges together, so that each commit of either of them is
-- reported once, where the range that still has it has it.
--
-- Both ranges are listed bottom commit first. A commit the new range doesn't
-- have is reported where the old range had it, and everything else where the
-- new range has it — so the old range has to be walked past the commits
-- already reported, which are the ones the new range moved down.
lineUp :: [Entry] -> [Entry] -> [Fate]
lineUp was now = go was now []
  where
    go os ns reported
      | (o : rest) <- unreported, isNothing (matching o now) = Dropped o : go rest ns reported
      | (n : ns') <- ns = case matching n was of
          Nothing -> PutIn n : go unreported ns' reported
          Just o -> Kept o n : go unreported ns' (enChangeId o : reported)
      | otherwise = []
      where
        unreported = dropWhile ((`elem` reported) . enChangeId) os

    matching e = find ((== enChangeId e) . enChangeId)

-- | What to say about one commit: a commit only one range has needs nothing
-- asked, and a commit both of them hold needs the interdiff from the one
-- version to the other.
--
-- A commit is reported with the sha of the side that still has it, and under
-- the message it goes under now — which for a reworded commit is the message
-- it was reworded to, not the one it is being rewritten away from.
report :: FilePath -> Fate -> IO Commit
report _ (Dropped o) = pure (Commit Removed (enCommitSha o) (enCommitMessage o))
report _ (PutIn n) = pure (Commit Added (enCommitSha n) (enCommitMessage n))
report dir (Kept o n) = (\c -> Commit c (enCommitSha n) (enCommitMessage n)) <$> change
  where
    change
      -- The same commit is the same patch, so there is nothing to ask jj.
      | enCommitSha o == enCommitSha n = pure Unchanged
      | otherwise = classify <$> interdiffOf dir (enCommitSha o) (enCommitSha n)
    -- A rewrite that left the commit's own diff alone -- a rebase, or the
    -- rebase a rewrite below it set off -- has nothing to show.
    classify "" = Unchanged
    classify patch = Updated (interdiff patch)

-- * Being somewhere jj can be asked

-- | Is the repo in @dir@ colocated: a jj repo and a git repo over the same
-- working copy, which is what a repo somebody works in with jj looks like?
--
-- This is the one question that says whose repo this is. A colocated repo is
-- somebody's own, kept by jj as they work, so jj already holds every commit
-- there is to ask about -- including the ones a rewrite left behind, which jj
-- remembers where git leaves them reachable from nothing. A repo that isn't is
-- a checkout something made to run this in, and nobody minds what is done to
-- it.
--
-- The check is for jj's directory beside git's, so @dir@ has to be the root of
-- the repo -- which is where this is run, and where a colocated repo keeps
-- both.
colocated :: FilePath -> IO Bool
colocated dir = doesDirectoryExist (dir ++ "/.jj")

-- | See to it that jj can be asked about the repo in @dir@, and that it holds
-- the given commits when it is.
--
-- A colocated repo is left exactly as it is. jj is already keeping it, so it
-- holds the commits already -- and it is somebody's own repo, which is reason
-- enough not to write anything to it that isn't asked for.
--
-- A repo that isn't colocated is made into one in place. It is a checkout
-- something made to run this in, so a @.jj@ beside its @.git@ costs nothing,
-- and jj reading the repo itself beats jj reading a copy of it: there is
-- nothing to copy the commits into and nothing to take down afterwards.
--
-- What it does need is a ref on each commit to ask about. jj only holds the
-- commits some ref leads it to, and the commit a force-push left behind is
-- reachable from nothing -- so without one it is a commit jj would say
-- doesn't exist. The refs go under a namespace jj reads: it takes in
-- refs/heads, refs/tags and refs/remotes, and nothing else. Each is named
-- after the commit it holds, so that asking about the same commit twice -- an
-- unmoved base, most often -- asks for the same ref twice rather than for two.
showJj :: FilePath -> [CommitSha] -> IO ()
showJj dir shas = do
  theirs <- colocated dir
  unless theirs $ do
    mapM_ keep shas
    void (jjRun dir ["git", "init", "--colocate"])
  where
    keep sha = void (gitCommand ["-C", dir, "update-ref", "refs/heads/rd-" ++ shaText sha, shaText sha])

-- | Run a git command, handing back what it printed on stdout.
gitCommand :: [String] -> IO String
gitCommand args = do
  (code, out, err) <- readProcessWithExitCode "git" args ""
  case code of
    ExitSuccess -> pure out
    ExitFailure _ -> fail ("git " ++ unwords args ++ " failed:\n" ++ trim err)

-- * Asking jj

-- | The commits of @base..head@, bottom commit first.
commitsIn :: FilePath -> CommitSha -> CommitSha -> IO [Entry]
commitsIn dir base head' =
  entries . lines
    <$> jjCommand
      dir
      [ "log",
        "--no-graph",
        "--reversed",
        "--revisions",
        shaText base ++ ".." ++ shaText head',
        "--template",
        template
      ]
  where
    -- One field per line, so that nothing has to be picked back out of a line
    -- shared with a commit message. A subject line is all a range-diff names a
    -- commit by, and it cannot hold a newline, so three lines is a commit.
    template = "change_id ++ \"\\n\" ++ commit_id ++ \"\\n\" ++ description.first_line() ++ \"\\n\""

    entries [] = []
    entries (c : s : m : rest) = Entry (ChangeId c) (knownSha s) (commitMessage m) : entries rest
    entries ls = error ("unexpected jj log output: " ++ unlines ls)

-- | What a rewrite did to one commit: the diff of what it did as @was@ against
-- what it does as @now@, which is what `jj interdiff` answers. Empty where the
-- two do the same thing.
--
-- jj counts a commit's description among its files, so a reword shows up here
-- as a diff of @JJ-COMMIT-DESCRIPTION@ rather than as nothing at all.
--
-- Asked for in git's own format, because that is the format whatever renders
-- this can be expected to know how to show. jj's own is nicer to read as it
-- stands, but it says which words of a line changed in colour alone, and there
-- is no colour to be had by the time this is being read.
interdiffOf :: FilePath -> CommitSha -> CommitSha -> IO String
interdiffOf dir was now =
  unlines . filter (not . isIndex) . lines
    <$> jjCommand dir ["interdiff", "--git", "--from", shaText was, "--to", shaText now]
  where
    -- The line naming the blobs either side of a change says nothing anyone
    -- reading a range-diff can act on. Only a header sits at the start of a
    -- line: every line of a diff's body opens with a space, a @+@, a @-@ or a
    -- backslash.
    isIndex = ("index " `isPrefixOf`)

-- | Run a jj command in the repo in @dir@, under the options everything asked
-- of jj here is asked under.
--
-- The repo is reached by running in it rather than by @--repository@: jj names
-- a file in a diff relative to the directory it was run in, and the root of
-- the repo is the one directory such a path reads as the path in the commit.
jjCommand :: FilePath -> [String] -> IO String
jjCommand dir args = jjRun dir (options ++ args)

-- | Run a jj command in the repo in @dir@, handing back what it printed on
-- stdout. jj says what it is doing on stderr, which is worth nothing while the
-- command works and is the whole story when it doesn't, so it is kept back
-- until one fails.
--
-- Bare of 'options', which is only good for a command that reads a repo:
-- making one doesn't take them.
jjRun :: FilePath -> [String] -> IO String
jjRun dir args = do
  (code, out, err) <- readCreateProcessWithExitCode (proc "jj" args) {cwd = Just dir} ""
  case code of
    ExitSuccess -> pure out
    ExitFailure _ -> fail ("jj " ++ unwords args ++ " failed:\n" ++ trim err)

-- | What every command here is run under, so that what jj prints is settled
-- here rather than by the jj config of whoever the tool is running as.
options :: [String]
options =
  [ -- An interdiff is text this hands on to be read somewhere that has no
    -- terminal to colour, so there is no colour to be had.
    "--color",
    "never",
    -- Taking a range-diff is reading the repo. Snapshotting the working copy,
    -- which jj would otherwise do first, would be rewriting it.
    "--ignore-working-copy"
  ]
    ++ concatMap asOption settings

settings :: [(String, String)]
settings =
  [ -- How much of a diff jj shows around a change, and whether it names a file
    -- as @a/f@ and @b/f@ the way git does, are both a matter of taste -- and
    -- this diff is not being shown to whoever's taste the config holds.
    ("diff.git.context", "3"),
    ("diff.git.show-path-prefix", "true")
  ]

asOption :: (String, String) -> [String]
asOption (k, v) = ["--config", k ++ "=" ++ v]
