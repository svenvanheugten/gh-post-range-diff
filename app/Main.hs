module Main where

import Data.List (unsnoc)
import GhPostRangeDiff.Git (commitSha)
import GhPostRangeDiff.GitHub (Ev (..), Ref (..), gh)
import GhPostRangeDiff.GitHub qualified as GitHub
import GhPostRangeDiff.Run (run)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Either just a PR number (manual use: report the most recent force-push),
  -- or a PR number plus the push's before/after SHAs from the pull_request
  -- payload (CI use: report exactly that push, whatever kind it is).
  args <- getArgs
  case args of
    [pr] -> manual (gh pr)
    [pr, before, after]
      | Just b <- commitSha before,
        Just a <- commitSha after ->
          run (gh pr) b a
      | otherwise -> manual (gh pr)
    _ -> error "usage: gh-post-range-diff <pr> [<before-sha> <after-sha>]"

-- Manual use: no SHAs on the command line, so derive them from the most recent
-- force-push in the timeline and hand off to `run`.
manual :: GitHub.Handle -> IO ()
manual pr = do
  heads <- filter ((== Head) . evRef) <$> GitHub.timeline pr
  case unsnoc heads of
    Nothing -> putStrLn "No force-push events on this PR. Nothing to diff."
    Just (_, h) -> run pr (evBefore h) (evAfter h)
