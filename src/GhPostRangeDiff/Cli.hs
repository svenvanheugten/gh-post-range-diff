-- | The CLI: what it makes of its arguments, and the report it runs on them.
module GhPostRangeDiff.Cli (cli) where

import Control.Monad (unless)
import Data.List (isPrefixOf, partition, unsnoc)
import GhPostRangeDiff.Colocate (colocatingAuto)
import GhPostRangeDiff.Git (abbrev)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ev (..), Ref (..))
import GhPostRangeDiff.GitHub qualified as GitHub
import GhPostRangeDiff.Render (UnchangedVisibility (..))
import GhPostRangeDiff.Run (Reported (..), run)
import RangeDiff qualified
import RangeDiff.CommitSha (CommitSha, commitSha)
import RangeDiff.Git qualified
import RangeDiff.Jj qualified

-- | Run the CLI on @args@ in the repo in @dir@, reaching a pull request by its
-- number through @github@, and hand back what it has to say, if anything.
cli :: (String -> GitHub.Handle) -> FilePath -> [String] -> IO (Maybe String)
cli github dir argv = do
  -- Either just a PR number (manual use: report the most recent force-push),
  -- or a PR number plus the push's before/after SHAs from the pull_request
  -- payload (CI use: report exactly that push, whatever kind it is). Either
  -- way, --hide-unchanged anywhere among them leaves the unchanged commits
  -- out of the report, and --no-jj takes the range-diff with git alone.
  let (flags, args) = partition ("--" `isPrefixOf`) argv
  unless (all (`elem` ["--hide-unchanged", "--no-jj"]) flags) usage
  -- A colocated repo is somebody's own, and already holds every commit there
  -- is to ask about, so nothing is fetched into it.
  theirs <- RangeDiff.Jj.colocated dir
  let repo = if theirs then Git.noFetching dir else Git.git dir
      visibility = if "--hide-unchanged" `elem` flags then HideUnchanged else ShowUnchanged
      rd = if "--no-jj" `elem` flags then RangeDiff.Git.git dir else colocatingAuto dir
  case args of
    [pr] -> manual repo rd (github pr) visibility
    [pr, before, after]
      | Just b <- commitSha before,
        Just a <- commitSha after ->
          report repo rd (github pr) visibility b a
      | otherwise -> manual repo rd (github pr) visibility
    _ -> usage

usage :: IO a
usage = error "usage: gh-post-range-diff [--hide-unchanged] [--no-jj] <pr> [<before-sha> <after-sha>]"

-- Manual use: no SHAs on the command line, so derive them from the most recent
-- force-push in the timeline and report on that one.
manual :: Git.Handle -> RangeDiff.Handle -> GitHub.Handle -> UnchangedVisibility -> IO (Maybe String)
manual repo rd pr visibility = do
  heads <- filter ((== Head) . evRef) <$> GitHub.timeline pr
  case unsnoc heads of
    Nothing -> pure (Just "No force-push events on this PR. Nothing to diff.")
    Just (_, h) -> report repo rd pr visibility (evBefore h) (evAfter h)

-- Report on a push, and say so where the run had nothing to do. Whatever the
-- range-diff takes is read out of @repo@, and the range-diff is taken with
-- @rd@.
report :: Git.Handle -> RangeDiff.Handle -> GitHub.Handle -> UnchangedVisibility -> CommitSha -> CommitSha -> IO (Maybe String)
report repo rd pr visibility oldHead newHead = do
  reported <- run repo rd pr visibility oldHead newHead
  pure $ case reported of
    Posted -> Nothing
    AlreadyReported ->
      Just ("Already reported on " ++ abbrev oldHead ++ ".." ++ abbrev newHead ++ ". Nothing to do.")
