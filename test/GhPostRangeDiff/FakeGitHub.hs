-- | A fake pull request on a repo.
module GhPostRangeDiff.FakeGitHub
  ( PullRequest,
    branch,
    newPullRequest,
    handle,
    push,
    comments,
  )
where

import Control.Monad (unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub qualified as GitHub
import RangeDiff.CommitSha (CommitSha)

data PullRequest = PullRequest
  { -- | the repo the pull request is on
    prOrigin :: Git.Handle,
    -- | where each of the two branches is now, which is what the next push
    -- is recorded against
    prBaseTip, prHeadTip :: IORef CommitSha,
    prTimeline :: IORef [GitHub.Ev],
    prComments :: IORef [String]
  }

-- | The branches the pull request is between: the one it targets, and the one
-- under review. Names of our choosing, so keep them clear of whatever else the
-- repo puts branches under.
branch :: GitHub.Ref -> String
branch GitHub.Base = "pr-base"
branch GitHub.Head = "pr-head"

-- | A pull request just opened on the repo @repo@ reads: where the 'branch' it
-- targets is, and where the 'branch' under review is. Both branches were
-- pushed before the pull request could exist, and neither of those pushes is
-- anything it records.
newPullRequest :: Git.Handle -> CommitSha -> CommitSha -> IO PullRequest
newPullRequest repo base head' =
  PullRequest repo
    <$> newIORef base
    <*> newIORef head'
    <*> newIORef []
    <*> newIORef []

-- Where one of the two branches is now.
tip :: PullRequest -> GitHub.Ref -> IORef CommitSha
tip pr GitHub.Base = prBaseTip pr
tip pr GitHub.Head = prHeadTip pr

-- | The 'GitHub.Handle' onto it, standing in for the `gh` CLI.
handle :: PullRequest -> GitHub.Handle
handle pr =
  GitHub.Handle
    { GitHub.timeline = readIORef (prTimeline pr),
      GitHub.baseRef = pure (branch GitHub.Base),
      GitHub.comments = unlines <$> readIORef (prComments pr),
      GitHub.postComment = \body -> modifyIORef' (prComments pr) (++ [body])
    }

-- | Record a push of a version of the branch, which has put the commit it
-- forks off on the 'branch' the pull request targets, and its tip on the
-- 'branch' under review.
--
-- Hand back the push to the branch under review, which is the one an action
-- fires on: 'Nothing' where that branch was already there, since a ref pushed
-- onto the sha it is already on moves nothing and no push happens at all.
push :: PullRequest -> CommitSha -> CommitSha -> IO (Maybe GitHub.Ev)
push pr base head' = do
  _ <- move pr GitHub.Base base
  move pr GitHub.Head head'

-- Record a push of a sha onto one of the two branches, and say what push came
-- of it. The timeline only remembers the push where it was not a
-- fast-forward, since an ordinary push leaves no force-push event behind.
move :: PullRequest -> GitHub.Ref -> CommitSha -> IO (Maybe GitHub.Ev)
move pr ref sha = do
  before <- readIORef (tip pr ref)
  if before == sha
    then pure Nothing
    else do
      let ev = GitHub.Ev ref before sha
      fastForward <- Git.isAncestorOf (prOrigin pr) before sha
      unless fastForward $ modifyIORef' (prTimeline pr) (++ [ev])
      writeIORef (tip pr ref) sha
      pure (Just ev)

-- | Its comments, oldest first.
comments :: PullRequest -> IO [String]
comments = readIORef . prComments
