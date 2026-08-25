module GhPostRangeDiff.GitCommand (git) where

import Control.Monad (void)
import System.Process (readProcess)

-- | Run a git command in the repo in @dir@. Nothing here reads what one prints,
-- but it is captured all the same, to keep it out of the test output.
git :: FilePath -> [String] -> IO ()
git dir args = void (readProcess "git" ("-C" : dir : args) "")
