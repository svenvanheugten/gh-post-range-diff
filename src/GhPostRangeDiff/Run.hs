-- | Reporting a push on a PR, as the CLI drives it.
module GhPostRangeDiff.Run (Reported (..), run) where

import Data.List (isInfixOf)
import GhPostRangeDiff.Comment (comment)
import GhPostRangeDiff.Git (CommitSha, shaText)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub qualified as GitHub

-- Hidden marker identifying this exact push (before..after). Lets us run the
-- program multiple times without duplicating comments.
marker :: CommitSha -> CommitSha -> String
marker oldHead newHead =
  "<!-- gh-post-range-diff " ++ shaText oldHead ++ ".." ++ shaText newHead ++ " -->"

-- | What reporting on a push came to: the comment went up, or the marker of
-- that push was on the PR already, so a previous run had said it all.
data Reported = Posted | AlreadyReported
  deriving (Eq, Show)

-- Report on the push oldHead..newHead: post what there is to say about it as a
-- PR comment, under the marker of that push, unless the marker says we already
-- have.
run :: Git.Handle -> GitHub.Handle -> CommitSha -> CommitSha -> IO Reported
run repo pr oldHead newHead = do
  posted <- GitHub.comments pr
  if marker oldHead newHead `isInfixOf` posted
    then pure AlreadyReported
    else do
      body <- comment repo pr oldHead newHead
      GitHub.postComment pr (marker oldHead newHead ++ "\n" ++ body)
      pure Posted
