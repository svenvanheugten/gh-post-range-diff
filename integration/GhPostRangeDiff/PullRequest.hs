-- | A pull request to run the CLI on: a fake one, on a real repo. The branch
-- under review is built in that repo, and the CLI runs in a clone of it, as it
-- does in a checkout in CI.
module GhPostRangeDiff.PullRequest
  ( PullRequest,
    inPullRequests,
    number,
    repo,
    root,
    base,
    push,
    checkout,
    cli,
    comments,
  )
where

import Control.Monad (void)
import GhPostRangeDiff.Cli qualified as Cli
import GhPostRangeDiff.FakeGitHub qualified as FakeGitHub
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ref (..))
import RangeDiff.CommitSha (CommitSha, shaText)
import RangeDiff.Test.Repo (ChangeId, Repo, configure, inRepos, initial, repoDir, sha, withEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess)
import Test.Hspec (Spec, SpecWith, aroundWith)

data PullRequest = PullRequest
  { -- | the repo it is on, in which the branch under review is built
    repo :: Repo,
    -- | the commit the branch it targets is on
    root :: ChangeId,
    prFake :: FakeGitHub.PullRequest
  }

-- | The number it goes by.
number :: String
number = "7"

-- | The clone of its repo the CLI runs in.
checkout :: PullRequest -> FilePath
checkout pr = takeDirectory (repoDir (repo pr)) </> "checkout"

-- | Run each example on a pull request of its own, just opened on a repo with
-- one commit in it, from a branch with nothing on it yet.
inPullRequests :: SpecWith PullRequest -> Spec
inPullRequests = inRepos . aroundWith withPullRequest

withPullRequest :: (PullRequest -> IO ()) -> Repo -> IO ()
withPullRequest act r = do
  -- GitHub hands out any commit that was ever pushed to it, whatever has been
  -- force-pushed over it since, and the CLI fetches the versions of the
  -- branch it reports on by their shas.
  configure r "uploadpack.allowAnySHA1InWant" "true"
  c <- initial r
  s <- sha r c
  place r s s
  fake <- FakeGitHub.newPullRequest (Git.git (repoDir r)) s s
  let pr = PullRequest r c fake
  git ["clone", "--quiet", "--no-checkout", repoDir r, checkout pr]
  withEnvironment [("GITHUB_ACTIONS", Nothing)] (act pr)

-- | The commit the branch the pull request targets is on.
base :: PullRequest -> IO CommitSha
base pr = sha (repo pr) (root pr)

-- | Push the branch under review with the commit @c@ on top, and hand back
-- where that put it.
push :: PullRequest -> ChangeId -> IO CommitSha
push pr c = do
  b <- base pr
  s <- sha (repo pr) c
  place (repo pr) b s
  _ <- FakeGitHub.push (prFake pr) b s
  pure s

-- | Run the CLI in the checkout, on @args@.
cli :: PullRequest -> [String] -> IO (Either String (Maybe String))
cli pr = Cli.cli (const (FakeGitHub.handle (prFake pr))) (checkout pr)

-- | The comments on the pull request, oldest first.
comments :: PullRequest -> IO [String]
comments = FakeGitHub.comments . prFake

-- Put the branch the pull request targets on @b@, and the branch under review
-- on @h@.
place :: Repo -> CommitSha -> CommitSha -> IO ()
place r b h = do
  git ["-C", repoDir r, "update-ref", "refs/heads/" ++ FakeGitHub.branch Base, shaText b]
  git ["-C", repoDir r, "update-ref", "refs/heads/" ++ FakeGitHub.branch Head, shaText h]

git :: [String] -> IO ()
git args = void (readProcess "git" args "")
