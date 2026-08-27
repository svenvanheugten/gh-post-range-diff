module RangeDiff.Test.Git (git, isAncestorOf, rangeDiff) where

import Control.Monad (void)
import RangeDiff.CommitSha (CommitSha, shaText)
import System.Exit (ExitCode (..))
import System.Process (readProcess, readProcessWithExitCode)

-- | Run a git command in the repo in @dir@. Nothing here reads what one prints,
-- but it is captured all the same, to keep it out of the test output.
git :: FilePath -> [String] -> IO ()
git dir args = void (readProcess "git" (at dir args) "")

-- | Does @a@ come before @b@ on the same line of history?
isAncestorOf :: FilePath -> CommitSha -> CommitSha -> IO Bool
isAncestorOf dir a b = do
  (code, _, _) <- readProcessWithExitCode "git" (at dir ["merge-base", "--is-ancestor", shaText a, shaText b]) ""
  pure (code == ExitSuccess)

-- | What `git range-diff` prints for @oldBase..oldHead@ against
-- @newBase..newHead@, for use in a counterexample.
rangeDiff :: FilePath -> CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO String
rangeDiff dir oldBase oldHead newBase newHead =
  readProcess "git" (at dir ["range-diff", range oldBase oldHead, range newBase newHead]) ""
  where
    range a b = shaText a ++ ".." ++ shaText b

-- Aim a git command at the repo in @dir@, so that which directory the process
-- happens to be in never comes into it.
at :: FilePath -> [String] -> [String]
at dir args = "-C" : dir : args
