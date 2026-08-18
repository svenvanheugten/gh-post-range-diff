-- | The `git` invocations the tool needs, and the base reconstruction on top of them.
module GhPostRangeDiff.Git
  ( sh,
    isAncestor,
    baseFor,
    fetch,
    revParse,
    rangeDiff,
  )
where

import Control.Monad.Extra (findM)
import Data.List.Extra (trim)
import System.Exit (ExitCode (..))
import System.Process (callProcess, readProcess, readProcessWithExitCode)

sh :: String -> [String] -> IO String
sh cmd args = readProcess cmd args ""

git :: [String] -> IO String
git = sh "git"

isAncestor :: String -> String -> IO Bool
isAncestor a b = do
  (code, _, _) <- readProcessWithExitCode "git" ["merge-base", "--is-ancestor", a, b] ""
  pure (code == ExitSuccess)

revParse :: String -> IO String
revParse rev = trim <$> git ["rev-parse", rev]

fetch :: [String] -> IO ()
fetch refs = callProcess "git" (["fetch", "--quiet", "origin"] ++ refs)

rangeDiff :: String -> String -> String -> String -> IO String
rangeDiff oldBase oldHead newBase newHead =
  git ["range-diff", oldBase ++ ".." ++ oldHead, newBase ++ ".." ++ newHead]

-- The base for `head`: the most recently recorded base tip that is still an
-- ancestor of it. `cands` needs to be in chronological order (current tip last).
--
-- If none is an ancestor (e.g. the base advanced through ordinary pushes, which
-- emits no force-push events) fall back to the merge-base with the current base
-- tip: the point where `head` forked from today's base line.
baseFor :: String -> String -> [String] -> IO String
baseFor currentBase headCommit cands = do
  found <- findM (`isAncestor` headCommit) (reverse cands)
  case found of
    Just b -> pure b
    Nothing -> trim <$> git ["merge-base", currentBase, headCommit]
