-- | The CLI as it is run: on a pull request whose branch is pushed and
-- force-pushed, the way the action runs it on every push and somebody at a
-- terminal runs it on the last force-push.
module GhPostRangeDiff.CliSpec (spec) where

import Control.Monad (void)
import GhPostRangeDiff.Git (abbrev)
import GhPostRangeDiff.PullRequest (PullRequest, base, checkout, cli, comments, inPullRequests, number, push, repo, root)
import GhPostRangeDiff.Render (UnchangedVisibility (..), format)
import RangeDiff qualified
import RangeDiff.Auto (auto)
import RangeDiff.CommitSha (CommitSha, shaText)
import RangeDiff.Git qualified
import RangeDiff.Jj qualified
import RangeDiff.Test.Repo (ChangeId, Patch (..))
import RangeDiff.Test.Repo qualified as Repo
import System.Process (readProcess)
import Test.Hspec

-- | Push the branch with the gadget on it, and hand that back.
gadget :: PullRequest -> IO (ChangeId, CommitSha)
gadget pr = do
  g <- Repo.new (repo pr) (root pr) (Patch "Add the gadget" [("gadget.txt", "The gadget.\n")])
  (,) g <$> push pr g

-- | Push the branch with the first take of the widget on top of the gadget,
-- and hand that back.
widget :: PullRequest -> ChangeId -> IO (ChangeId, CommitSha)
widget pr g = do
  w <- Repo.new (repo pr) g (Patch "Add the widget" [("widget.txt", take' 1)])
  (,) w <$> push pr w

-- | Force-push the branch with take @n@ of the widget, and hand back where it
-- is.
rework :: PullRequest -> ChangeId -> Int -> IO CommitSha
rework pr w n = do
  Repo.amend (repo pr) w [("widget.txt", take' n)]
  push pr w

take' :: Int -> String
take' n = "The widget, take " ++ show n ++ ".\n"

-- | The comment on the push old..new: its marker, a header naming the push,
-- and the range-diff of the two versions of the branch it is between, with the
-- unchanged commits shown or left out as @visibility@ says, taken with @rd@.
report :: (FilePath -> RangeDiff.Handle) -> PullRequest -> UnchangedVisibility -> CommitSha -> CommitSha -> IO String
report rd pr visibility old new = do
  b <- base pr
  commits <- RangeDiff.rangeDiff (rd (checkout pr)) b old b new
  pure $
    unlines
      [ "<!-- gh-post-range-diff " ++ shaText old ++ ".." ++ shaText new ++ " -->",
        "### Range-diff for push " ++ abbrev old ++ " → " ++ abbrev new
      ]
      ++ "\n"
      ++ format visibility commits

usage :: String
usage = "usage: gh-post-range-diff [--hide-unchanged] [--no-jj] <pr> [<before-sha> <after-sha>]"

spec :: Spec
spec = inPullRequests $ do
  describe "given a push" $ do
    it "posts the range-diff of that push" $ \pr -> do
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      cli pr [number, shaText w1, shaText w2] `shouldReturn` Right Nothing
      expected <- report auto pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]

    it "posts the range-diff of a push that only added commits" $ \pr -> do
      (g, g1) <- gadget pr
      (_, w1) <- widget pr g
      cli pr [number, shaText g1, shaText w1] `shouldReturn` Right Nothing
      expected <- report auto pr ShowUnchanged g1 w1
      comments pr `shouldReturn` [expected]

    it "posts it only once, however often it is run" $ \pr -> do
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      let args = [number, shaText w1, shaText w2]
      cli pr args `shouldReturn` Right Nothing
      cli pr args `shouldReturn` Right (Just ("Already reported on " ++ abbrev w1 ++ ".." ++ abbrev w2 ++ ". Nothing to do."))
      expected <- report auto pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]

    it "leaves the unchanged commits out with --hide-unchanged" $ \pr -> do
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      cli pr ["--hide-unchanged", number, shaText w1, shaText w2] `shouldReturn` Right Nothing
      expected <- report auto pr HideUnchanged w1 w2
      comments pr `shouldReturn` [expected]

    it "takes the range-diff with jj in GitHub Actions" $ \pr -> do
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      Repo.withEnvironment [("GITHUB_ACTIONS", Just "true")] (cli pr [number, shaText w1, shaText w2]) `shouldReturn` Right Nothing
      expected <- report RangeDiff.Jj.jj pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]

    it "takes the range-diff with jj in GitHub Actions in a checkout that already is a colocated jj repo" $ \pr -> do
      _ <- Repo.colocate (checkout pr)
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      Repo.withEnvironment [("GITHUB_ACTIONS", Just "true")] (cli pr [number, shaText w1, shaText w2]) `shouldReturn` Right Nothing
      expected <- report RangeDiff.Jj.jj pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]

    it "takes the range-diff with git alone with --no-jj in GitHub Actions, and leaves the checkout that way" $ \pr -> do
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      Repo.withEnvironment [("GITHUB_ACTIONS", Just "true")] (cli pr ["--no-jj", number, shaText w1, shaText w2]) `shouldReturn` Right Nothing
      expected <- report RangeDiff.Git.git pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]
      RangeDiff.Jj.colocated (checkout pr) `shouldReturn` False

    it "takes the range-diff with git alone in GitHub Actions where there is no jj" $ \pr -> do
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      Repo.withEnvironment [("GITHUB_ACTIONS", Just "true")] (Repo.withoutJj (cli pr [number, shaText w1, shaText w2])) `shouldReturn` Right Nothing
      expected <- report RangeDiff.Git.git pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]

    it "takes the range-diff with jj outside GitHub Actions in somebody's own jj repo, and fetches nothing into it" $ \pr -> do
      _ <- Repo.colocate (checkout pr)
      let pull = void (readProcess "jj" ["-R", checkout pr, "git", "fetch", "--quiet"] "")
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      pull
      w2 <- rework pr w 2
      pull
      cli pr [number, shaText w1, shaText w2] `shouldReturn` Right Nothing
      expected <- report RangeDiff.Jj.jj pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]
      readProcess "git" ["-C", checkout pr, "for-each-ref", "refs/rd"] "" `shouldReturn` ""

    it "takes the range-diff with git alone outside GitHub Actions in a repo that isn't colocated" $ \pr -> do
      (g, _) <- gadget pr
      (w, w1) <- widget pr g
      w2 <- rework pr w 2
      cli pr [number, shaText w1, shaText w2] `shouldReturn` Right Nothing
      expected <- report RangeDiff.Git.git pr ShowUnchanged w1 w2
      comments pr `shouldReturn` [expected]

  describe "given only the pull request" $ do
    it "posts the range-diff of the last force-push" $ \pr -> do
      (g, _) <- gadget pr
      (w, _) <- widget pr g
      w2 <- rework pr w 2
      w3 <- rework pr w 3
      cli pr [number] `shouldReturn` Right Nothing
      expected <- report auto pr ShowUnchanged w2 w3
      comments pr `shouldReturn` [expected]

    it "says so where nothing was force-pushed" $ \pr -> do
      (g, _) <- gadget pr
      _ <- widget pr g
      cli pr [number] `shouldReturn` Right (Just "No force-push events on this PR. Nothing to diff.")
      comments pr `shouldReturn` []

    it "leaves the unchanged commits out with --hide-unchanged after the number" $ \pr -> do
      (g, _) <- gadget pr
      (w, _) <- widget pr g
      w2 <- rework pr w 2
      w3 <- rework pr w 3
      cli pr [number, "--hide-unchanged"] `shouldReturn` Right Nothing
      expected <- report auto pr HideUnchanged w2 w3
      comments pr `shouldReturn` [expected]

  it "hands back its usage on arguments it can't make out" $ \pr -> do
    cli pr [] `shouldReturn` Left usage
    comments pr `shouldReturn` []

  it "hands back its usage on a flag it doesn't know" $ \pr -> do
    cli pr ["--hide-unchanged-commits", number] `shouldReturn` Left usage
    comments pr `shouldReturn` []

  it "hands back its usage on a before or after that isn't a sha" $ \pr -> do
    (g, _) <- gadget pr
    (w, w1) <- widget pr g
    _ <- rework pr w 2
    cli pr [number, "not-a-sha", shaText w1] `shouldReturn` Left usage
    cli pr [number, shaText w1, "not-a-sha"] `shouldReturn` Left usage
    comments pr `shouldReturn` []
