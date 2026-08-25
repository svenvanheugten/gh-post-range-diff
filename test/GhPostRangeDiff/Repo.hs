-- | A repo whose branch is rewritten over and over, the way force-pushing one
-- rewrites it.
--
-- What happens to the repo is not decided here: it is drawn as a
-- "GhPostRangeDiff.Plan", which is planned in full before the repo exists.
-- 'withRepo' draws a plan and builds a repo holding the first version of its
-- branch; 'evolve' then carries out every rewrite the plan holds. An evolve
-- knows what each commit was rewritten into, which is what a range-diff says,
-- so it can hand one back.
module GhPostRangeDiff.Repo
  ( Repo,
    repoDir,
    repoGit,
    withRepo,
    Rewrite (..),
    evolve,
    Range (..),
    range,
    Movement (..),
    baseMovement,
  )
where

import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (find, intercalate, unsnoc)
import Data.List.Extra (trim)
import Data.Maybe (fromMaybe, isNothing)
import GhPostRangeDiff.Git (CommitSha, knownSha, shaText)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitCommand (git)
import GhPostRangeDiff.Plan (Action (..), Committed (..), Plan (..), Step (..), plan, shrinkPlan)
import GhPostRangeDiff.RangeDiff qualified as RangeDiff
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (Property, Testable, forAllShrink, ioProperty, suchThat)

-- * The repo a scenario is carried out in

-- | jj's own name for a commit, which survives every rewrite of it: a reword,
-- an amend, and the rebase either of those sets off above it. It is how a
-- scenario finds a commit again however the versions since have rewritten it.
newtype ChangeId = ChangeId {changeIdText :: String}
  deriving (Eq)

-- | One commit of the branch, as the repo has it.
data Commit = Commit
  { cmChange :: ChangeId,
    -- | what its file is called, which is a file no other commit ever touches
    cmName :: String,
    cmCommitted :: Committed
  }

-- | What the repo holds: the commit every version grows from, how many commits
-- it has ever been given (which is where the next one's name comes from), and
-- the branch as it stands — the commits under the fork point and the commits
-- above it, each bottom commit first.
data State = State
  { stRoot :: CommitSha,
    stNamed :: Int,
    stBase, stBranch :: [Commit]
  }

-- | A repo a scenario is being carried out in.
data Repo = Repo
  { -- | where it is, so that whatever else is done with it (opening a pull
    -- request on it, cloning it) can be done from anywhere
    repoDir :: FilePath,
    -- | the rewrites still to come, in order
    rpSteps :: [Step],
    rpState :: IORef State
  }

-- | The repo behind a git handle, which is what everything reading it with git
-- goes through.
repoGit :: Repo -> Git.Handle
repoGit = Git.git . repoDir

-- | Plan a scenario of between @lo@ and @hi@ rewrites, make a repo holding the
-- first version of its branch, and run the property in it. Everything the
-- property goes on to do runs with that repo as the working directory.
--
-- Only plans @fit@ takes are built, and only those are shrunk to, so a property
-- that can't be run on every scenario says so here rather than throwing the
-- repo away once it has been built. "GhPostRangeDiff.Plan"'s @unambiguous@ is
-- the one such thing there is to ask.
withRepo :: (Testable prop) => (Int, Int) -> (Plan -> Bool) -> (Repo -> IO prop) -> Property
withRepo bounds fit act =
  forAllShrink (plan bounds `suchThat` fit) (filter fit . shrinkPlan) $ \p ->
    ioProperty $ withSystemTempDirectory "gh-post-range-diff" $ \dir ->
      withCurrentDirectory dir $ do
        initRepo dir
        root <- commitRoot
        st <- newIORef (State root 0 [] [])
        let repo = Repo dir (plSteps p) st
        initialize repo p
        act repo

-- | Commit the first version of the branch: the commits the plan puts under the
-- fork point, and the commits it puts above it.
initialize :: Repo -> Plan -> IO ()
initialize repo p = do
  root <- stRoot <$> readIORef (rpState repo)
  built <- apply repo (shaText root) [] (map Insert (plBase p ++ plBranch p))
  let (below, above) = splitAt (length (plBase p)) built
  modifyIORef' (rpState repo) $ \st -> st {stBase = below, stBranch = above}

-- | Where the branch begins and where it ends, as the repo has it now.
data Range = Range
  { -- | the commit the branch forks off, which is where its range is taken from
    rgBase :: CommitSha,
    -- | its tip
    rgHead :: CommitSha
  }

-- | The range the branch spans as the repo has it now. This is what a
-- range-diff is taken over, and what a push moves the two refs of a pull
-- request onto.
range :: Repo -> IO Range
range repo = do
  st <- readIORef (rpState repo)
  v <- versionOf (stBase st ++ stBranch st)
  let tipOf = fmap (shaOf v . snd) . unsnoc
  pure
    Range
      { rgBase = fromMaybe (stRoot st) (tipOf (stBase st)),
        rgHead = fromMaybe (error "the branch has no commits") (tipOf (stBranch st))
      }

-- | What the base under the branch did between where it was and where it is.
-- Only good for saying what a scenario covered.
data Movement
  = -- | it is where it was: the version was pushed onto the base the one before
    -- it was pushed onto
    Stayed
  | -- | it grew, so it got where it is by an ordinary push, which leaves no
    -- force-push event to reconstruct it from
    Advanced
  | -- | it moved off the line it was on, which takes a force-push, and that
    -- leaves an event behind
    Forced
  deriving (Eq, Show)

baseMovement :: Git.Handle -> CommitSha -> CommitSha -> IO Movement
baseMovement repo was now
  | was == now = pure Stayed
  | otherwise = do
      fastForward <- Git.isAncestorOf repo was now
      pure (if fastForward then Advanced else Forced)

-- | One rewrite of the branch, as it happened: where the branch was, where it
-- is now, and the range-diff from the one version to the other.
data Rewrite = Rewrite
  { rwWas, rwNow :: Range,
    rwRangeDiff :: [RangeDiff.Commit]
  }

-- | Rewrite the branch into the next version of itself, once for every rewrite
-- the plan holds, and hand each one to @act@.
--
-- Each is handed over before the next is made, so that whatever @act@ does about
-- a push has only what that push left behind to go on — the way anything
-- reporting on a pull request sees only the timeline as it stood at the time.
evolve :: Repo -> (Rewrite -> IO a) -> IO [a]
evolve repo act = mapM one (rpSteps repo)
  where
    one s = do
      from <- range repo
      st <- readIORef (rpState repo)
      was <- versionOf (stBranch st)
      base <- apply repo (shaText (stRoot st)) (stBase st) (stpBase s)
      branch <- apply repo (fromMaybe (shaText (stRoot st)) (revOf base)) (stBranch st) (stpBranch s)
      modifyIORef' (rpState repo) $ \s' -> s' {stBase = base, stBranch = branch}
      now <- versionOf branch
      to <- range repo
      act (Rewrite from to (rangeDiffOf was now))

    revOf = fmap (changeIdText . cmChange . snd) . unsnoc

-- * Doing it to the repo

-- | Read a plan's actions against the stretch of branch they were planned for,
-- bottom commit first, and hand back the stretch they leave behind. @below@ is
-- the revision that stretch sits on: what a commit put in at the bottom of it
-- goes after.
--
-- This is "GhPostRangeDiff.Plan"'s @applied@ carried out for real, and the two
-- have to stay in step.
apply :: Repo -> String -> [Commit] -> [Action] -> IO [Commit]
apply _ _ cs [] = pure cs
apply repo below cs (a : as) = case a of
  Insert d -> do
    c <- insert repo below d
    above c cs
  Keep -> next above
  Rework d -> next $ \c rest -> do
    rework c d
    above c {cmCommitted = d} rest
  -- Abandoning a commit rebases whatever sat above it onto whatever sat below
  -- it, so the branch closes over the gap.
  Drop -> next $ \c rest -> do
    _ <- jj ["abandon", changeIdText (cmChange c)]
    apply repo below rest as
  where
    above c rest = (c :) <$> apply repo (changeIdText (cmChange c)) rest as

    next f = case cs of
      [] -> error "plan acts on a commit that isn't there"
      c : rest -> f c rest

-- | Put a commit in that wasn't there, straight above @below@.
insert :: Repo -> String -> Committed -> IO Commit
insert repo below d = do
  name <- fresh repo
  _ <- jj ["new", "--insert-after", below, message (cdMessage d)]
  -- The new commit is the one jj left the working copy on, so writing the file
  -- is all it takes to put it in the commit: the next jj command snapshots the
  -- working copy, and the one reading the change id back is that command.
  writeFile (file name) (contents name (cdLine d))
  c <- ChangeId . trim <$> jj ["log", "--no-graph", "-r", "@", "-T", "change_id"]
  pure (Commit c name d)

-- | Rewrite a commit into what the next version makes of it: a reword goes
-- straight onto the commit, and an amend goes through the working copy, since
-- checking a commit out and writing the file is the only way to change what it
-- holds.
rework :: Commit -> Committed -> IO ()
rework c d = do
  when (cdMessage (cmCommitted c) /= cdMessage d) $
    void (jj ["describe", "-r", rev, message (cdMessage d)])
  when (cdLine (cmCommitted c) /= cdLine d) $ do
    _ <- jj ["edit", rev]
    writeFile (file (cmName c)) (contents (cmName c) (cdLine d))
  where
    rev = changeIdText (cmChange c)

-- | A name no commit in the repo has been given before, for the file the next
-- one commits to.
fresh :: Repo -> IO String
fresh repo = atomicModifyIORef' (rpState repo) $ \st ->
  (st {stNamed = stNamed st + 1}, "f" ++ show (stNamed st + 1))

-- | A stretch of branch as it is at some point: the commits it is made of, and
-- the commit each of them is on.
data Version = Version
  { vsCommits :: [Commit],
    vsShas :: [(ChangeId, CommitSha)]
  }

-- | Read what commits a stretch of branch is on as the repo stands.
versionOf :: [Commit] -> IO Version
versionOf [] = pure (Version [] [])
versionOf cs = Version cs . map entry . lines <$> jj ["log", "--no-graph", "-r", revset, "-T", template]
  where
    revset = intercalate " | " [changeIdText (cmChange c) | c <- cs]
    template = "change_id ++ \" \" ++ commit_id ++ \"\\n\""
    entry l = case words l of
      [c, sha] -> (ChangeId c, knownSha sha)
      _ -> error ("unexpected jj log line: " ++ l)

-- | The commit one of them is on.
shaOf :: Version -> Commit -> CommitSha
shaOf v c = fromMaybe (error ("jj lost " ++ cmName c)) (lookup (cmChange c) (vsShas v))

-- * The range-diff the rewrite amounts to

-- | The range-diff from the branch as it was to the branch as it is: what
-- happened to every commit of either version, in the order `git range-diff`
-- prints them, with the commit each of them is on.
--
-- Which commit of the old version is which commit of the new one is jj's
-- business, and git works it out for itself, by how alike two commits' patches
-- are. The two agree because every commit touches a file of its own, and only
-- ever one line of it, so a rewritten commit is far more like what it was than
-- like anything else in either version.
rangeDiffOf :: Version -> Version -> [RangeDiff.Commit]
rangeDiffOf was now = go (vsCommits was) (vsCommits now) []
  where
    -- git walks the two versions at once, printing a commit the new version
    -- doesn't have where the old one has it, and every commit the old version
    -- doesn't have where the new one has it. A commit both of them have is
    -- printed where the new version has it, which is why the old version has to
    -- be walked past the ones already printed.
    go os ns shown
      | (o : rest) <- unshown, isNothing (matching o new) = dropped o : go rest ns shown
      | (n : ns') <- ns = case matching n old of
          Nothing -> introduced n : go unshown ns' shown
          Just o -> paired o n : go unshown ns' (cmChange o : shown)
      | otherwise = []
      where
        unshown = dropWhile ((`elem` shown) . cmChange) os

    matching c = find ((== cmChange c) . cmChange)

    old = vsCommits was
    new = vsCommits now

    -- git takes the message on a header line from the old side of every pair it
    -- lines up, so a reworded commit is reported under the message it is being
    -- rewritten away from.
    paired o n
      | cmCommitted o == cmCommitted n = entry RangeDiff.Unchanged (shaOf now n) n
      | otherwise = entry (RangeDiff.Updated (interdiff o n)) (shaOf now n) o

    -- A dropped commit is only on the old side, so that is the side it is
    -- reported from.
    dropped o = entry RangeDiff.Removed (shaOf was o) o
    introduced n = entry RangeDiff.Added (shaOf now n) n

    entry change sha c = RangeDiff.Commit change sha (cdMessage (cmCommitted c))

-- | The interdiff git prints under a rewritten commit, as
-- 'RangeDiff.rangeDiff' should hand it back: de-indented, so its own +/- sit in
-- column 0. A rewrite that reworded the commit gets a hunk over the metadata
-- git shows a commit under, one that amended it gets a hunk over the patch, and
-- one that did both gets both hunks, in that order.
interdiff :: Commit -> Commit -> RangeDiff.Interdiff
interdiff a b = RangeDiff.interdiff (unlines (reworded ++ amended))
  where
    (was, now) = (cmCommitted a, cmCommitted b)
    name = cmName b

    -- The message sits in a section of its own, indented by four, with the
    -- author above it and the head of the patch below — three lines of context
    -- either side, as a default @-U3@ diff leaves.
    reworded
      | cdMessage was == cdMessage now = []
      | otherwise =
          [ "@@ Metadata",
            " Author: " ++ auName author ++ " <" ++ auEmail author ++ ">",
            " ",
            "  ## Commit message ##",
            "-    " ++ RangeDiff.messageText (cdMessage was),
            "+    " ++ RangeDiff.messageText (cdMessage now),
            " ",
            "  ## " ++ file name ++ " (new) ##",
            " @@"
          ]

    -- Context here is the tail of the file's body, as the new patch adds it.
    amended
      | cdLine was == cdLine now = []
      | otherwise =
          ["@@ " ++ file name ++ " (new)"]
            ++ [" +" ++ body name k | k <- [6 .. 8]]
            ++ ["-+" ++ cdLine was, "++" ++ cdLine now]

-- * The repo underneath

-- | The file a commit commits to. One file each, so unrelated commits can never
-- be mistaken for one another.
file :: String -> FilePath
file name = name ++ ".txt"

-- | The @k@th body line of a commit's file. Naming the commit it belongs to
-- keeps two commits' patches from resembling one another, which would let
-- range-diff pair a dropped commit with an unrelated one that was put in.
body :: String -> Int -> String
body name k = name ++ "-l" ++ show k

-- | A commit's file, ending on the line the version carrying it chose. The body
-- is long enough that rewriting that one line is a small fraction of the
-- commit, which keeps range-diff pairing the two versions of it (marker @!@)
-- instead of treating them as an unrelated drop plus an unrelated insert.
contents :: String -> String -> String
contents name l = unlines ([body name k | k <- [1 .. 8]] ++ [l])

-- | Who commits everything in the repo. The scenario needs it too: it is the
-- one line of commit metadata a range-diff shows around a reworded message.
data Author = Author
  { auName :: String,
    auEmail :: String
  }

author :: Author
author = Author {auName = "t", auEmail = "t@t"}

-- | Run jj under the settings every repo here is built with, handing back what
-- it printed on stdout. jj narrates what it rewrote on stderr, which is noise
-- while the build goes to plan and the whole story when it doesn't, so it is
-- kept back until a command fails.
jj :: [String] -> IO String
jj args = do
  (code, out, err) <- readProcessWithExitCode "jj" (concatMap asOption settings ++ args) ""
  case code of
    ExitSuccess -> pure out
    ExitFailure _ -> fail ("jj " ++ unwords args ++ " failed:\n" ++ err)
  where
    asOption (k, v) = ["--config", k ++ "=" ++ v]

-- | Everything the build leans on, settled here rather than left to the jj
-- config of whoever is running the tests.
--
-- Handed to every command rather than written to the repo's own config: jj
-- keeps repo-level config under the user's config directory, keyed by the path
-- of the repo, so writing it needs a home to write to — which a sandboxed
-- build hasn't got — and would leave an entry behind for every throwaway repo
-- the tests build. Options on the command line need no home at all, and win
-- over a user's config just the same.
settings :: [(String, String)]
settings =
  [ ("user.name", auName author),
    ("user.email", auEmail author),
    -- Every file a commit writes is a file it means to commit.
    ("snapshot.auto-track", "all()"),
    -- Rewriting history is what these repos are for, so nothing in them is
    -- history jj should be holding on to.
    ("revset-aliases.\"immutable_heads()\"", "none()")
  ]

-- | How a message is handed to jj.
message :: RangeDiff.CommitMessage -> String
message m = "--message=" ++ RangeDiff.messageText m

-- | A repo to rewrite a branch in. It is colocated, so that everything reading
-- it afterwards — `git range-diff`, and the checkout the tool runs in — is
-- reading an ordinary git repo.
initRepo :: FilePath -> IO ()
initRepo dir = do
  _ <- jj ["git", "init", "--colocate", "."]
  -- How far git abbreviates a sha is its own business, and it varies with the
  -- size of the repo. Told not to abbreviate at all, `range-diff` prints the
  -- full shas, which are the ones the scenario knows its commits by.
  git dir ["config", "core.abbrev", "no"]

-- | The commit every version of the branch grows from, and the one a version
-- with nothing under its fork point forks off. jj made a commit to initialise
-- the repo on; this is that commit, given something to hold and something to
-- say.
commitRoot :: IO CommitSha
commitRoot = do
  writeFile "root.txt" "root\n"
  _ <- jj ["describe", "--message=root"]
  knownSha . trim <$> jj ["log", "--no-graph", "-r", "@", "-T", "commit_id"]
