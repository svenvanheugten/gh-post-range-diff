-- | Where a version of a branch forks off the base branch it was pushed
-- against, worked back out of the pull request's timeline however far the base
-- branch has moved since.
module GhPostRangeDiff.Bases (Bases (..), bases) where

import Control.Monad.Extra (andM, findM, notM)
import Data.List (nub)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ev (..), Ref (..))
import GhPostRangeDiff.GitHub qualified as GitHub
import RangeDiff.CommitSha (CommitSha)

-- | Where each of the two versions of the branch forks off, which is where the
-- range-diff between them is taken from.
data Bases = Bases
  { -- | the base the version being replaced was on
    basesOld :: CommitSha,
    -- | the base the version left behind is on
    basesNew :: CommitSha
  }
  deriving (Eq, Show)

-- | Find where the two versions of the branch either side of the push
-- oldHead..newHead fork off.
bases :: Git.Handle -> GitHub.Handle -> CommitSha -> CommitSha -> IO Bases
bases repo pr oldHead newHead = do
  base <- GitHub.baseRef pr
  -- Every recorded base tip, in chronological order, for base reconstruction.
  recorded <-
    concatMap (\e -> [evBefore e, evAfter e]) . filter ((== Base) . evRef)
      <$> GitHub.timeline pr

  -- Fetch current base tip, both heads, and every historical base oid.
  newBaseTip <- Git.fetch repo base (nub (oldHead : newHead : recorded))
  if null recorded
    then Bases <$> mergeBase newBaseTip oldHead <*> mergeBase newBaseTip newHead
    else do
      lostAncestorOfOldHead <-
        findM
          (\tl -> andM [tl `isAncestorOf` oldHead, notM (tl `isAncestorOf` newBaseTip)])
          (reverse recorded)
      b1 <- maybe (mergeBase newBaseTip oldHead) pure lostAncestorOfOldHead
      pure (Bases b1 newBaseTip)
  where
    isAncestorOf = Git.isAncestorOf repo
    mergeBase = Git.mergeBase repo
