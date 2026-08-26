-- | What a push gets reported with: everything it takes to work out what the
-- push did, and the Markdown saying so.
module GhPostRangeDiff.Comment (comment) where

import Control.Monad.Extra (andM, findM, notM)
import Data.List (nub)
import GhPostRangeDiff.Git (CommitSha, abbrev, fetch, isAncestorOf, mergeBase, revParse, shaText)
import GhPostRangeDiff.GitHub (Ev (..), Ref (..))
import GhPostRangeDiff.GitHub qualified as GitHub
import GhPostRangeDiff.RangeDiff (rangeDiff)
import GhPostRangeDiff.Render (format)

-- | Report on the push oldHead..newHead: a header naming both ends of it, and
-- the range-diff between the version of the branch it replaced and the one it
-- left behind.
comment :: GitHub.Handle -> CommitSha -> CommitSha -> IO String
comment pr oldHead newHead = do
  base <- GitHub.baseRef pr
  -- Every recorded base tip, in chronological order, for base reconstruction.
  forcePushesToBase <-
    nub . concatMap (\e -> [evBefore e, evAfter e]) . filter ((== Base) . evRef)
      <$> GitHub.timeline pr

  -- Fetch current base tip, both heads, and every historical base oid.
  --
  -- refs/rd/base is a scratch ref holding the fetched base tip. We need it
  -- because without a destination the tip only lands in FETCH_HEAD, which
  -- this same fetch also fills with the heads and every base oid, so we
  -- couldn't pick the base tip back out to rev-parse on the next line.
  fetch $ (base ++ ":refs/rd/base") : map shaText (oldHead : newHead : forcePushesToBase)
  newBaseTip <- revParse "refs/rd/base"
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
  commits <- rangeDiff b1 oldHead b2 newHead

  let header = "### Range-diff for push " ++ abbrev oldHead ++ " → " ++ abbrev newHead
  pure (header ++ "\n\n" ++ format commits)
