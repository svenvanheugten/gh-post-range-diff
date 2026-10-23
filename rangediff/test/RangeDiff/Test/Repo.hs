-- | Throwaway repos to take range-diffs in, whose branches are rewritten the
-- way force-pushing one rewrites it.
--
-- Commits are made and rewritten with jj, and the repo is colocated, so the
-- same history can be read with git. A rewrite really is one: a reworded,
-- amended, abandoned or moved commit keeps its change id, and the commits
-- above it are rebased onto what it became. That keeps the history something a
-- range-diff by change id could be taken over, as well as one by patch, which
-- is all `git range-diff` goes on.
module RangeDiff.Test.Repo
  ( Repo,
    inRepos,
    configure,
    ChangeId,
    Patch (..),
    initial,
    new,
    stack,
    insertAfter,
    reword,
    amend,
    abandon,
    rebase,
    moveBefore,
    sha,
    shas,
    RangeDiffOf,
    rangeDiff,
    shouldRead,
  )
where

import Control.Exception (ErrorCall (..), Handler (..), bracket, catches, throwIO)
import Control.Monad (foldM, void)
import Data.List.Extra (trim)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import RangeDiff qualified
import RangeDiff.CommitSha (CommitSha, knownSha, shaText)
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess, readProcessWithExitCode)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (Expectation, Spec, SpecWith, around, aroundAll_, expectationFailure, shouldBe)

-- | A repo to build a branch in, by where it is.
newtype Repo = Repo FilePath

-- | A commit by its change id, which survives every rewrite of it.
newtype ChangeId = ChangeId String

-- | What one commit says and what it writes: its message, and the files it
-- writes, each with its contents in full.
data Patch = Patch
  { ptMessage :: String,
    ptFiles :: [(FilePath, String)]
  }

-- | Run each example in a repo of its own, which goes away afterwards.
inRepos :: SpecWith Repo -> Spec
inRepos = aroundAll_ isolated . around withRepo

-- | Run the examples in an environment of their own. What git prints is the
-- thing under test, so the config of whoever runs the tests has no business
-- shaping it, nor that of jj what gets built: a global excludes file, say,
-- would keep files out of the commits. The environment is inherited by every
-- process the examples run, the git behind 'RangeDiff.git' included, and is
-- put back the way it was once they are done.
isolated :: IO () -> IO ()
isolated act = withSystemTempDirectory "gh-post-range-diff" $ \tmp -> do
  writeFile (tmp </> "jj.toml") jjConfig
  let environment =
        [ ("HOME", Just tmp),
          -- Where git and jj look for config, excludes and attributes that
          -- nothing points them at.
          ("XDG_CONFIG_HOME", Just (tmp </> "config")),
          ("GIT_CONFIG_GLOBAL", Just "/dev/null"),
          ("GIT_CONFIG_NOSYSTEM", Just "1"),
          ("GIT_CONFIG_PARAMETERS", Nothing),
          ("GIT_CONFIG_COUNT", Nothing),
          ("JJ_CONFIG", Just (tmp </> "jj.toml")),
          ("JJ_USER", Nothing),
          ("JJ_EMAIL", Nothing)
        ]
  bracket (swap environment) swap (const act)
  where
    -- Set each variable, handing back what it was set to before.
    swap = mapM $ \(k, v) -> do
      was <- lookupEnv k
      maybe (unsetEnv k) (setEnv k) v
      pure (k, was)

jjConfig :: String
jjConfig =
  unlines
    [ "user.name = \"t\"",
      "user.email = \"t@t\"",
      "# Every file a commit writes is a file it means to commit.",
      "snapshot.auto-track = \"all()\"",
      "# Rewriting history is what these repos are for, so nothing in them is",
      "# history jj should be holding on to.",
      "revset-aliases.\"immutable_heads()\" = \"none()\""
    ]

-- | Run an example in a repo of its own, which goes away afterwards.
withRepo :: (Repo -> IO a) -> IO a
withRepo act = withSystemTempDirectory "repo" $ \tmp -> do
  let dir = tmp </> "repo"
  createDirectory dir
  _ <- jj ["git", "init", "--colocate", dir]
  act (Repo dir)

-- | Set a git config variable in the repo itself, where it outranks whatever
-- the environment sets.
configure :: Repo -> String -> String -> IO ()
configure (Repo dir) k v = void (readProcess "git" ["-C", dir, "config", k, v] "")

-- | The commit everything else grows from. jj made a commit to initialise the
-- repo on; this is that commit, given something to hold and something to say.
initial :: Repo -> IO ChangeId
initial repo@(Repo dir) = do
  write repo [("root.txt", "root\n")]
  _ <- jjIn dir ["describe", "--message=root"]
  current repo

-- | A new commit of @p@ on top of @parent@.
new :: Repo -> ChangeId -> Patch -> IO ChangeId
new repo@(Repo dir) (ChangeId parent) p = do
  _ <- jjIn dir ["new", parent, messageOption (ptMessage p)]
  write repo (ptFiles p)
  current repo

-- | A run of new commits on top of @base@, bottom one first.
stack :: Repo -> ChangeId -> [Patch] -> IO [ChangeId]
stack repo base = fmap reverse . foldM on []
  where
    on built p = (: built) <$> new repo (topOr built) p
    topOr (c : _) = c
    topOr [] = base

-- | Put a commit of @p@ in straight above @below@, with whatever sat on
-- @below@ rebased onto it.
insertAfter :: Repo -> ChangeId -> Patch -> IO ChangeId
insertAfter repo@(Repo dir) (ChangeId below) p = do
  _ <- jjIn dir ["new", "--insert-after", below, messageOption (ptMessage p)]
  write repo (ptFiles p)
  current repo

-- | Give a commit a new message.
reword :: Repo -> ChangeId -> String -> IO ()
reword (Repo dir) (ChangeId c) m = void (jjIn dir ["describe", "-r", c, messageOption m])

-- | Rewrite files in a commit. Checking it out and writing them is the only way
-- to change what a commit holds.
amend :: Repo -> ChangeId -> [(FilePath, String)] -> IO ()
amend repo@(Repo dir) (ChangeId c) files = do
  _ <- jjIn dir ["edit", c]
  write repo files
  void (current repo)

-- | Take a commit out of the branch, closing the gap over it.
abandon :: Repo -> ChangeId -> IO ()
abandon (Repo dir) (ChangeId c) = void (jjIn dir ["abandon", c])

-- | Move a commit, and everything above it, onto @onto@.
rebase :: Repo -> ChangeId -> ChangeId -> IO ()
rebase (Repo dir) (ChangeId c) (ChangeId onto) = void (jjIn dir ["rebase", "-s", c, "-d", onto])

-- | Move a commit on its own to just under @above@.
moveBefore :: Repo -> ChangeId -> ChangeId -> IO ()
moveBefore (Repo dir) (ChangeId c) (ChangeId above) = void (jjIn dir ["rebase", "-r", c, "--insert-before", above])

-- | The commit a change is on as the repo stands, which moves with every
-- rewrite of it or of anything under it.
sha :: Repo -> ChangeId -> IO CommitSha
sha repo c = do
  [s] <- shas repo [c]
  pure s

-- | The commits some distinct changes are on, in the order they are asked for.
-- Each is abbreviated as git abbreviates it, since that is how `range-diff`
-- prints it to the tool.
shas :: Repo -> [ChangeId] -> IO [CommitSha]
shas (Repo dir) cs = do
  ids <- map (break (== ' ')) . lines <$> jjIn dir ["log", "--no-graph", "-r", revset, "-T", template]
  let full (ChangeId c) = drop 1 (fromMaybe (error ("no commit for change " ++ c)) (lookup c ids))
  map knownSha . lines
    <$> readProcess "git" (["-C", dir, "log", "--no-walk=unsorted", "--format=%h"] ++ map full cs) ""
  where
    revset = foldr1 (\a b -> a ++ " | " ++ b) [c | ChangeId c <- cs]
    template = "change_id ++ \" \" ++ commit_id ++ \"\\n\""

-- | The range-diff of @oldBase..oldHead@ against @newBase..newHead@, still to
-- be taken.
data RangeDiffOf = RangeDiffOf Repo (CommitSha, CommitSha) (CommitSha, CommitSha)

rangeDiff :: Repo -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> RangeDiffOf
rangeDiff = RangeDiffOf

-- | Expect a range-diff, taken the way the tool takes it, to read as
-- @expected@. The expected values are written out by hand from what git
-- prints, so when one doesn't match, or git prints something that can't be
-- read at all, the failure says what git printed.
shouldRead :: (HasCallStack) => RangeDiffOf -> [RangeDiff.Commit] -> Expectation
shouldRead (RangeDiffOf repo@(Repo dir) (oldBase, oldHead) (newBase, newHead)) expected =
  (RangeDiff.rangeDiff (RangeDiff.git dir) oldBase oldHead newBase newHead >>= (`shouldBe` expected))
    `catches` [ Handler $ \(HUnitFailure loc reason) -> throwIO . HUnitFailure loc . withPrinted reason =<< printed,
                Handler $ \(ErrorCall e) -> expectationFailure . ((e ++ "\n") ++) =<< printed
              ]
  where
    printed = ("git range-diff printed:\n" ++) <$> rangeDiffText repo (range oldBase oldHead) (range newBase newHead)
    range a b = shaText a ++ ".." ++ shaText b
    withPrinted (ExpectedButGot preface e g) p = ExpectedButGot (Just (maybe "" (++ "\n") preface ++ p)) e g
    withPrinted (Reason r) p = Reason (r ++ "\n" ++ p)

-- | What `git range-diff` prints for two ranges, as a person would read it.
rangeDiffText :: Repo -> String -> String -> IO String
rangeDiffText (Repo dir) before after = readProcess "git" ["-C", dir, "range-diff", "--no-color", before, after] ""

-- | Write files into the working copy. Whatever commit jj has it on gets them
-- the next time jj runs, which snapshots it.
write :: Repo -> [(FilePath, String)] -> IO ()
write (Repo dir) = mapM_ $ \(path, contents) -> do
  createDirectoryIfMissing True (takeDirectory (dir </> path))
  writeFile (dir </> path) contents

-- | The commit the working copy is on, snapshotting what was written to it.
current :: Repo -> IO ChangeId
current (Repo dir) = ChangeId . trim <$> jjIn dir ["log", "--no-graph", "-r", "@", "-T", "change_id"]

messageOption :: String -> String
messageOption m = "--message=" ++ m

-- | Run jj in the repo in @dir@, handing back what it printed on stdout.
jjIn :: FilePath -> [String] -> IO String
jjIn dir args = jj (["-R", dir] ++ args)

-- | Run jj, handing back what it printed on stdout. jj narrates what it
-- rewrote on stderr, which is noise while a build goes to plan and the whole
-- story when it doesn't, so it is kept back until a command fails.
jj :: [String] -> IO String
jj args = do
  (code, out, err) <- readProcessWithExitCode "jj" args ""
  case code of
    ExitSuccess -> pure out
    ExitFailure _ -> fail ("jj " ++ unwords args ++ " failed:\n" ++ err)
