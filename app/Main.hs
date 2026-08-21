module Main where

import GhPostRangeDiff.Git (commitSha)
import GhPostRangeDiff.Run (manual, run)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Either just a PR number (manual use: report the most recent force-push),
  -- or a PR number plus the push's before/after SHAs from the pull_request
  -- payload (CI use: report exactly that push, whatever kind it is).
  args <- getArgs
  case args of
    [pr] -> manual pr
    [pr, before, after]
      | Just b <- commitSha before,
        Just a <- commitSha after ->
          run pr b a
      | otherwise -> manual pr
    _ -> error "usage: gh-post-range-diff <pr> [<before-sha> <after-sha>]"
