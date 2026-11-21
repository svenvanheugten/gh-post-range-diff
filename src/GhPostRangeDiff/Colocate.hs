-- | Making the checkout the tool runs in into a colocated jj repo, so that
-- 'RangeDiff.Auto.auto' can take the range-diff with jj there.
module GhPostRangeDiff.Colocate (colocatingAuto) where

import Control.Monad (void, when)
import Data.List.Extra (trim)
import RangeDiff qualified
import RangeDiff.Auto (auto)
import RangeDiff.CommitSha (CommitSha, shaText)
import RangeDiff.Git qualified
import RangeDiff.Jj qualified
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

-- | 'auto', after making the checkout in @dir@ into a colocated repo that
-- holds the commits, where there is a jj to do that.
colocatingAuto :: FilePath -> RangeDiff.Handle
colocatingAuto dir = RangeDiff.Handle {RangeDiff.rangeDiff = colocatingRangeDiff}
  where
    colocatingRangeDiff oldBase oldHead newBase newHead = do
      installed <- RangeDiff.Jj.installed
      when installed $ colocate dir [oldBase, oldHead, newBase, newHead]
      RangeDiff.rangeDiff (auto dir) oldBase oldHead newBase newHead

-- | Make the checkout in @dir@ into a colocated repo that holds the given
-- commits.
--
-- It is a checkout made for the tool to run in, so it is made into one in
-- place, and there is nothing to take down afterwards. Where something made
-- it into one already -- an earlier run, or an earlier step of the workflow --
-- jj is only told to take in the commits.
--
-- Either way, it needs a ref on each commit to ask about. jj only holds the
-- commits some ref leads it to, and the commit a force-push left behind is
-- reachable from nothing -- so without one it is a commit jj would say
-- doesn't exist. The refs go under a namespace jj reads: it takes in
-- refs/heads, refs/tags and refs/remotes, and nothing else.
colocate :: FilePath -> [CommitSha] -> IO ()
colocate dir shas = do
  mapM_ keep shas
  already <- RangeDiff.Jj.colocated dir
  void (jjIn dir (if already then ["git", "import"] else ["git", "init", "--colocate"]))
  where
    keep sha = void (RangeDiff.Git.command dir ["update-ref", "refs/heads/rd-" ++ shaText sha, shaText sha])

-- Run a jj command in the repo in @dir@, handing back what it printed on
-- stdout, and what it said on stderr where it failed.
jjIn :: FilePath -> [String] -> IO String
jjIn dir args = do
  (code, out, err) <- readCreateProcessWithExitCode (proc "jj" args) {cwd = Just dir} ""
  case code of
    ExitSuccess -> pure out
    ExitFailure _ -> fail ("jj " ++ unwords args ++ " failed:\n" ++ trim err)
