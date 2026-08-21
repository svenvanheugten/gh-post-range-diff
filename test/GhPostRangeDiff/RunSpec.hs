-- | Reporting on every push a scenario describes, by replaying it onto a pull
-- request opened on the scenario's own repo and letting the tool loose on it
-- the way the action does.
module GhPostRangeDiff.RunSpec (spec) where

import Control.Monad (forM)
import Data.List (intercalate, isPrefixOf, tails, unsnoc)
import Data.Maybe (catMaybes)
import GhPostRangeDiff.FakeGitHub (PullRequest)
import GhPostRangeDiff.FakeGitHub qualified as FakeGitHub
import GhPostRangeDiff.Git (shaText)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ev (..))
import GhPostRangeDiff.GitHub qualified as GitHub
import GhPostRangeDiff.RangeDiff qualified as RangeDiff
import GhPostRangeDiff.Render (format)
import GhPostRangeDiff.Run (Reported (..), run)
import GhPostRangeDiff.Scenario
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Discard (Discard), Gen, Property, choose, conjoin, counterexample, forAllShrink, ioProperty, property, tabulate, (===))

-- * The pull request the pushes land on

-- | A pull request just opened on the scenario's repo, on the version the
-- branch was first built as. Both branches had to be pushed before there was a
-- pull request to open, so it starts out knowing where each of them is.
--
-- The pull request plants a branch per ref in the repo, which is what a
-- checkout of it has to fetch. Those names have to stay clear of the branch per
-- version that 'withRepo' creates.
newPullRequest :: Repo -> IO PullRequest
newPullRequest repo = do
  let Range base head' = rangeOf repo (Version 0)
  FakeGitHub.newPullRequest (repoDir repo) base head'

-- * The pushes a scenario describes

-- | Push the version @v@ of the branch, as far as GitHub can see it: the base
-- branch ends up where that version was built on, the branch under review on
-- its tip, and the timeline records whichever of the two moves was a
-- force-push.
--
-- Hand back the push to the branch under review, which is the one the action
-- fires on and the before/after it is handed, or 'Nothing' where the branch
-- came out exactly as it already was, so nothing was pushed and nothing fired.
push :: PullRequest -> Repo -> Version -> IO (Maybe GitHub.Ev)
push pr repo v = FakeGitHub.push pr base head'
  where
    Range base head' = rangeOf repo v

-- | Report on a push where the action reports on it: against the pull request
-- as GitHub has it so far, in a repo of its own that has fetched nothing yet,
-- with the repo the pull request is on as its origin. Whatever the tool needs
-- to diff, it has to go and get.
report :: PullRequest -> GitHub.Ev -> IO Reported
report pr ev =
  withSystemTempDirectory "gh-post-range-diff-checkout" $ \dir -> withCurrentDirectory dir $ do
    _ <- git ["init", "-q"]
    _ <- git ["remote", "add", "origin", FakeGitHub.origin pr]
    -- The scenario knows its commits by their full shas, so the range-diff
    -- taken here has to name them the same way the scenario's repo does.
    _ <- git ["config", "core.abbrev", "no"]
    run (FakeGitHub.handle pr) (evBefore ev) (evAfter ev)

-- * The property a spec runs on a scenario

-- | Say what the replay a property ran on turned out to cover: how many pushes
-- it made, and what each of them did to the base under the branch. Each push is
-- named by the version of the branch it replaced and the one it left behind.
coverage :: Repo -> [(Version, Version)] -> Property -> IO Property
coverage repo ps prop = do
  bases <- sequence [baseMovement repo v w | (v, w) <- ps]
  pure (tabulate "pushes" [show (length ps)] (tabulate "base" bases prop))

-- | Some situations are ambiguous, and we need to avoid those.
--
-- Say a version commits @A@ and then @B@ under the fork point, and a later one
-- drops @B@: that later base is an ancestor of the earlier head too, and the
-- newer of the two, so the tool takes it as the base commit for both the old
-- and new version. The old version then starts at @A@, with @B@ a commit of
-- the branch rather than one under it.
unambiguous :: Repo -> Scenario -> IO Bool
unambiguous repo sc = and <$> sequence [older v w | (v, w) <- laterVersions]
  where
    laterVersions = [(v, w) | (v : ws) <- tails (versions sc), w <- ws]
    older v w
      | base w == base v = pure True
      | otherwise = not <$> Git.isAncestor (base w) (rgHead (rangeOf repo v))
    base = rgBase . rangeOf repo

-- | Scenarios of a handful of versions, so that a replay has several pushes to
-- walk and several bases to tell apart.
scenario :: Gen Scenario
scenario = choose (2, 4) >>= scenarioOf

-- * What a push should get reported with

-- | What a comment says, which is everything below whatever the tool hid at the
-- top of it. An HTML comment renders as nothing, so what one holds is no part
-- of what the comment says.
said :: String -> String
said = intercalate "\n" . dropWhile ("<!--" `isPrefixOf`) . lines

-- | What a push should get reported with: a header naming both ends of it, and
-- the range-diff between the two versions of the branch, taken from the bases
-- the scenario built them on.
expected :: Repo -> (Version, Version) -> IO String
expected repo (v, w) = do
  let Range oldBase oldHead = rangeOf repo v
      Range newBase newHead = rangeOf repo w
  commits <- RangeDiff.rangeDiff oldBase oldHead newBase newHead
  pure $
    "### Range-diff for push "
      ++ take 7 (shaText oldHead)
      ++ " → "
      ++ take 7 (shaText newHead)
      ++ "\n\n"
      ++ format commits

spec :: Spec
spec =
  -- Every case builds a repo and then a checkout per push, so keep the number
  -- of them small.
  modifyMaxSuccess (const 20) $
    describe "run" $
      it "posts the range-diff of every push once" $
        forAllShrink scenario shrinkScenario $ \sc -> ioProperty $ withRepo sc $ \repo -> do
          ok <- unambiguous repo sc
          if not ok
            then pure (property Discard)
            else do
              let vs = versions sc
              pr <- newPullRequest repo
              -- Each push is reported on as it happens, so the tool only ever
              -- sees the timeline GitHub had recorded by then.
              pushes <- fmap catMaybes . forM (zip vs (drop 1 vs)) $ \(v, w) -> do
                pushed <- push pr repo w
                forM pushed $ \ev -> do
                  r <- report pr ev
                  pure ((v, w), ev, r)
              posted <- FakeGitHub.comments pr
              -- The newest push, reported on a second time: the tool has said
              -- this once already, so it has nothing left to say. A scenario
              -- whose every version came out as the one before it describes no
              -- push at all, and then there is nothing to report on again
              -- either.
              again <- forM (unsnoc pushes) $ \(_, (_, ev, _)) -> report pr ev
              posted' <- FakeGitHub.comments pr
              want <- mapM (\(p, _, _) -> expected repo p) pushes
              coverage repo [p | (p, _, _) <- pushes] $
                conjoin
                  [ counterexample "one comment per push, saying what that push did" (map said posted === want),
                    counterexample "every push reported" ([r | (_, _, r) <- pushes] === map (const Posted) pushes),
                    counterexample "reporting a push again" (again === (AlreadyReported <$ unsnoc pushes)),
                    counterexample "which posts nothing" (posted' === posted)
                  ]
