-- | What a push gets reported with: everything it takes to work out what the
-- push did, and the Markdown saying so.
module GhPostRangeDiff.Comment (comment) where

import Control.Monad.Extra (andM, findM, notM)
import Data.List (nub)
import GhPostRangeDiff.Git (abbrev)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ev (..), Ref (..))
import GhPostRangeDiff.GitHub qualified as GitHub
import GhPostRangeDiff.Render (format)
import RangeDiff qualified
import RangeDiff.CommitSha (CommitSha)

-- | Report on the push oldHead..newHead: a header naming both ends of it, and
-- the range-diff between the version of the branch it replaced and the one it
-- left behind.
comment :: Git.Handle -> RangeDiff.Handle -> GitHub.Handle -> CommitSha -> CommitSha -> IO String
comment repo rd pr oldHead newHead = do
  base <- GitHub.baseRef pr
  -- Every recorded base tip, in chronological order, for base reconstruction.
  forcePushesToBase <-
    concatMap (\e -> [evBefore e, evAfter e]) . filter ((== Base) . evRef)
      <$> GitHub.timeline pr

  -- Fetch current base tip, both heads, and every historical base oid.
  newBaseTip <- Git.fetch repo base (nub (oldHead : newHead : forcePushesToBase))
  (b1, b2) <-
    if null forcePushesToBase
      then do
        b1 <- mergeBase newBaseTip oldHead
        b2 <- mergeBase newBaseTip newHead
        pure (b1, b2)
      else do
        lostAncestorOfOldHead <-
          findM
            (\tl -> andM [tl `isAncestorOf` oldHead, notM (tl `isAncestorOf` newBaseTip)])
            (reverse forcePushesToBase)
        b1 <- maybe (mergeBase newBaseTip oldHead) pure lostAncestorOfOldHead
        pure (b1, newBaseTip)
  commits <- RangeDiff.rangeDiff rd b1 oldHead b2 newHead

  let header = "### Range-diff for push " ++ abbrev oldHead ++ " → " ++ abbrev newHead
  pure (header ++ "\n\n" ++ format commits)
  where
    isAncestorOf = Git.isAncestorOf repo
    mergeBase = Git.mergeBase repo
