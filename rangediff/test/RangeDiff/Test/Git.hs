-- | Plain git, for a spec that builds a repo without jj and so has nothing
-- carrying a change id.
module RangeDiff.Test.Git (git, revParse) where

import Control.Monad (void)
import Data.List.Extra (trim)
import RangeDiff.CommitSha (CommitSha, knownSha)
import System.Process (readProcess)

-- | Run a git command in the repo in @dir@. Nothing here reads what one prints,
-- but it is captured all the same, to keep it out of the test output.
git :: FilePath -> [String] -> IO ()
git dir args = void (readProcess "git" (at dir args) "")

-- | The commit a revision names, so that a spec building a repo with git can
-- read back what it built.
revParse :: FilePath -> String -> IO CommitSha
revParse dir rev = knownSha . trim <$> readProcess "git" (at dir ["rev-parse", rev]) ""

-- Aim a git command at the repo in @dir@, so that which directory the process
-- happens to be in never comes into it.
at :: FilePath -> [String] -> [String]
at dir args = "-C" : dir : args
