-- | The CLI: what it makes of its arguments, and the report it runs on them.
module GhPostRangeDiff.Cli (cli) where

import Data.List (isPrefixOf, partition, unsnoc)
import GhPostRangeDiff.Git (abbrev)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ev (..), Ref (..))
import GhPostRangeDiff.GitHub qualified as GitHub
import GhPostRangeDiff.Render (UnchangedVisibility (..))
import GhPostRangeDiff.Run (Reported (..), run)
import RangeDiff.Auto (auto)
import RangeDiff.CommitSha (CommitSha, commitSha)

-- | Run the CLI on @args@ in the repo in @dir@, reaching a pull request by its
-- number through @github@, and hand back what it has to say, if anything.
cli :: (String -> GitHub.Handle) -> FilePath -> [String] -> IO (Maybe String)
cli github dir argv = do
  -- Either just a PR number (manual use: report the most recent force-push),
  -- or a PR number plus the push's before/after SHAs from the pull_request
  -- payload (CI use: report exactly that push, whatever kind it is). Either
  -- way, --hide-unchanged anywhere among them leaves the unchanged commits
  -- out of the report.
  let (flags, args) = partition ("--" `isPrefixOf`) argv
  visibility <- case flags of
    [] -> pure ShowUnchanged
    ["--hide-unchanged"] -> pure HideUnchanged
    _ -> usage
  case args of
    [pr] -> manual dir (github pr) visibility
    [pr, before, after]
      | Just b <- commitSha before,
        Just a <- commitSha after ->
          report dir (github pr) visibility b a
      | otherwise -> manual dir (github pr) visibility
    _ -> usage

usage :: IO a
usage = error "usage: gh-post-range-diff [--hide-unchanged] <pr> [<before-sha> <after-sha>]"

-- Manual use: no SHAs on the command line, so derive them from the most recent
-- force-push in the timeline and report on that one.
manual :: FilePath -> GitHub.Handle -> UnchangedVisibility -> IO (Maybe String)
manual dir pr visibility = do
  heads <- filter ((== Head) . evRef) <$> GitHub.timeline pr
  case unsnoc heads of
    Nothing -> pure (Just "No force-push events on this PR. Nothing to diff.")
    Just (_, h) -> report dir pr visibility (evBefore h) (evAfter h)

-- Report on a push, and say so where the run had nothing to do. Whatever the
-- range-diff takes is read out of the repo in @dir@.
report :: FilePath -> GitHub.Handle -> UnchangedVisibility -> CommitSha -> CommitSha -> IO (Maybe String)
report dir pr visibility oldHead newHead = do
  reported <- run (Git.git dir) (auto dir) pr visibility oldHead newHead
  pure $ case reported of
    Posted -> Nothing
    AlreadyReported ->
      Just ("Already reported on " ++ abbrev oldHead ++ ".." ++ abbrev newHead ++ ". Nothing to do.")
