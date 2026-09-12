-- | The repo the tool takes its range-diffs in, behind a handle, so that it can
-- be mocked in the tests. The one implementation that talks to a real repo is
-- 'git', which is that repo's directory and the `git` invocations it takes to
-- read it.
module GhPostRangeDiff.Git
  ( abbrev,
    Handle (..),
    git,
  )
where

import Data.List.Extra (trim)
import RangeDiff.CommitSha (CommitSha, knownSha, shaText)
import RangeDiff.Jj (colocated)
import System.Exit (ExitCode (..))
import System.Process (callProcess, readProcess, readProcessWithExitCode)

-- | A sha cut down to the seven digits people read commits by. Only for
-- showing: it is git's own default abbreviation, not necessarily unambiguous.
abbrev :: CommitSha -> String
abbrev = take 7 . shaText

-- | Everything the tool does with the repo it takes its range-diffs in.
data Handle = Handle
  { -- | See to it that the repo has the branch named here and every commit
    -- listed, bringing them in from origin where it doesn't, and hand back
    -- the commit that branch is on. Nothing else here, and no range-diff, can
    -- be asked about a commit this has not seen to first.
    fetch :: String -> [CommitSha] -> IO CommitSha,
    -- | Does @a@ come before @b@ on the same line of history?
    isAncestorOf :: CommitSha -> CommitSha -> IO Bool,
    -- | The best common ancestor of @a@ and @b@.
    mergeBase :: CommitSha -> CommitSha -> IO CommitSha
  }

-- | The repo in @dir@, as the `git` CLI reaches it.
git :: FilePath -> Handle
git dir =
  Handle
    { fetch = gitFetch dir,
      isAncestorOf = gitIsAncestorOf dir,
      mergeBase = gitMergeBase dir
    }

-- Run a git command in the repo in @dir@, and hand back what it printed on
-- stdout.
command :: FilePath -> [String] -> IO String
command dir args = readProcess "git" (at dir args) ""

-- Aim a git command at the repo in @dir@. Every invocation here is aimed this
-- way, so which directory the process happens to be in never comes into it.
at :: FilePath -> [String] -> [String]
at dir args = "-C" : dir : args

-- Make sure the repo has both versions of the branch, and read back where the
-- base branch ended up.
--
-- A colocated repo -- one somebody works in with jj -- is left alone. It is
-- their own repo, and it already holds every commit there is to ask about:
-- they pushed both versions of the branch from it, and jj keeps what a rewrite
-- leaves behind where git leaves it reachable from nothing. So there is
-- nothing to bring in, and fetching would only mean leaving a ref of ours
-- behind in somebody's repo. A repo that isn't colocated is a checkout
-- something made to run this in, and nothing is lost by writing to it. See
-- 'colocated'.
--
-- refs/rd/base is a scratch ref holding the fetched branch tip. We need it
-- because without a destination the tip only lands in FETCH_HEAD, which this
-- same fetch also fills with every commit asked for, so we couldn't pick the
-- tip back out to rev-parse afterwards.
gitFetch :: FilePath -> String -> [CommitSha] -> IO CommitSha
gitFetch dir base commits = do
  theirs <- colocated dir
  if theirs
    then revParse dir ("refs/remotes/origin/" ++ base)
    else do
      callProcess "git" (at dir (["fetch", "--quiet", "origin", base ++ ":refs/rd/base"] ++ map shaText commits))
      revParse dir "refs/rd/base"

gitIsAncestorOf :: FilePath -> CommitSha -> CommitSha -> IO Bool
gitIsAncestorOf dir a b = do
  (code, _, _) <- readProcessWithExitCode "git" (at dir ["merge-base", "--is-ancestor", shaText a, shaText b]) ""
  pure (code == ExitSuccess)

gitMergeBase :: FilePath -> CommitSha -> CommitSha -> IO CommitSha
gitMergeBase dir a b = knownSha . trim <$> command dir ["merge-base", shaText a, shaText b]

-- The commit a revision names.
revParse :: FilePath -> String -> IO CommitSha
revParse dir rev = knownSha . trim <$> command dir ["rev-parse", rev]
