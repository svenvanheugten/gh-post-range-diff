-- | The CLI: what it makes of its arguments, and the report it runs on them.
module GhPostRangeDiff.Cli (cli) where

import Data.List (unsnoc)
import GhPostRangeDiff.Git (abbrev)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ev (..), Ref (..))
import GhPostRangeDiff.GitHub qualified as GitHub
import GhPostRangeDiff.Run (Reported (..), run)
import RangeDiff.Auto (auto)
import RangeDiff.CommitSha (CommitSha, commitSha)

-- | Run the CLI on @args@ in the repo in @dir@, reaching a pull request by its
-- number through @github@, and hand back what it has to say, if anything.
cli :: (String -> GitHub.Handle) -> FilePath -> [String] -> IO (Maybe String)
cli github dir args =
  -- Either just a PR number (manual use: report the most recent force-push),
  -- or a PR number plus the push's before/after SHAs from the pull_request
  -- payload (CI use: report exactly that push, whatever kind it is).
  case args of
    [pr] -> manual dir (github pr)
    [pr, before, after]
      | Just b <- commitSha before,
        Just a <- commitSha after ->
          report dir (github pr) b a
      | otherwise -> manual dir (github pr)
    _ -> error "usage: gh-post-range-diff <pr> [<before-sha> <after-sha>]"

-- Manual use: no SHAs on the command line, so derive them from the most recent
-- force-push in the timeline and report on that one.
manual :: FilePath -> GitHub.Handle -> IO (Maybe String)
manual dir pr = do
  heads <- filter ((== Head) . evRef) <$> GitHub.timeline pr
  case unsnoc heads of
    Nothing -> pure (Just "No force-push events on this PR. Nothing to diff.")
    Just (_, h) -> report dir pr (evBefore h) (evAfter h)

-- Report on a push, and say so where the run had nothing to do. Whatever the
-- range-diff takes is read out of the repo in @dir@.
report :: FilePath -> GitHub.Handle -> CommitSha -> CommitSha -> IO (Maybe String)
report dir pr oldHead newHead = do
  reported <- run (Git.git dir) (auto dir) pr oldHead newHead
  pure $ case reported of
    Posted -> Nothing
    AlreadyReported ->
      Just ("Already reported on " ++ abbrev oldHead ++ ".." ++ abbrev newHead ++ ". Nothing to do.")
