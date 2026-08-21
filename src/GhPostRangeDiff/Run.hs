-- | Reporting a push on a PR, as the CLI drives it.
module GhPostRangeDiff.Run (run, manual) where

import Data.List (isInfixOf, nub, unsnoc)
import GhPostRangeDiff.Git (CommitSha, abbrev, baseFor, fetch, revParse, shaText)
import GhPostRangeDiff.GitHub (Ev (..), baseRef, comments, postComment, timeline)
import GhPostRangeDiff.RangeDiff (rangeDiff)
import GhPostRangeDiff.Render (format)

-- Report on the push oldHead..newHead: post its range-diff as a PR comment.
run :: String -> CommitSha -> CommitSha -> IO ()
run pr oldHead newHead = do
  base <- baseRef pr
  -- Every recorded base tip, in chronological order, for base reconstruction.
  baseOids <-
    nub . concatMap (\e -> [evBefore e, evAfter e]) . filter ((== "BaseRefForcePushedEvent") . evType)
      <$> timeline pr

  -- Hidden marker identifying this exact push (before..after). Lets us run the
  -- program multiple times without duplicating comments.
  let marker = "<!-- gh-post-range-diff " ++ shaText oldHead ++ ".." ++ shaText newHead ++ " -->"

  posted <- comments pr
  if marker `isInfixOf` posted
    then
      putStrLn ("Already reported on " ++ abbrev oldHead ++ ".." ++ abbrev newHead ++ ". Nothing to do.")
    else do
      -- Fetch current base tip, both heads, and every historical base oid.
      --
      -- refs/rd/base is a scratch ref holding the fetched base tip. We need it
      -- because without a destination the tip only lands in FETCH_HEAD, which
      -- this same fetch also fills with the heads and every base oid, so we
      -- couldn't pick the base tip back out to rev-parse on the next line.
      fetch $ (base ++ ":refs/rd/base") : map shaText (oldHead : newHead : baseOids)
      newBaseTip <- revParse "refs/rd/base"
      let cands = baseOids ++ [newBaseTip] -- current tip is newest, so it goes last
      b1 <- baseFor newBaseTip oldHead cands
      b2 <- baseFor newBaseTip newHead cands
      commits <- rangeDiff b1 oldHead b2 newHead

      let header = "### Range-diff for push " ++ abbrev oldHead ++ " → " ++ abbrev newHead
      postComment pr (marker ++ "\n" ++ header ++ "\n\n" ++ format commits)

-- Manual use: no SHAs on the command line, so derive them from the most recent
-- force-push in the timeline and hand off to `run`.
manual :: String -> IO ()
manual pr = do
  heads <- filter ((== "HeadRefForcePushedEvent") . evType) <$> timeline pr
  case unsnoc heads of
    Nothing -> putStrLn "No force-push events on this PR. Nothing to diff."
    Just (_, h) -> run pr (evBefore h) (evAfter h)
