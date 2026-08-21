-- | Reporting a push on a PR, as the CLI drives it.
module GhPostRangeDiff.Run (run) where

import Data.List (isInfixOf)
import GhPostRangeDiff.Comment (comment)
import GhPostRangeDiff.Git (CommitSha, abbrev, shaText)
import GhPostRangeDiff.GitHub qualified as GitHub

-- Hidden marker identifying this exact push (before..after). Lets us run the
-- program multiple times without duplicating comments.
marker :: CommitSha -> CommitSha -> String
marker oldHead newHead =
  "<!-- gh-post-range-diff " ++ shaText oldHead ++ ".." ++ shaText newHead ++ " -->"

-- Report on the push oldHead..newHead: post what there is to say about it as a
-- PR comment, under the marker of that push, unless the marker says we already
-- have.
run :: GitHub.Handle -> CommitSha -> CommitSha -> IO ()
run pr oldHead newHead = do
  posted <- GitHub.comments pr
  if marker oldHead newHead `isInfixOf` posted
    then
      putStrLn ("Already reported on " ++ abbrev oldHead ++ ".." ++ abbrev newHead ++ ". Nothing to do.")
    else do
      body <- comment pr oldHead newHead
      GitHub.postComment pr (marker oldHead newHead ++ "\n" ++ body)
