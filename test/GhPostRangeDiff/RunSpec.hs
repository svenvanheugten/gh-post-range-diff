-- | Reporting on every push a branch goes through, by opening a pull request on
-- the repo the branch is being rewritten in, pushing every version of it, and
-- letting the tool loose on the pull request the way the action does.
module GhPostRangeDiff.RunSpec (spec) where

import Data.List (intercalate, isPrefixOf)
import GhPostRangeDiff.FakeGitHub (PullRequest)
import GhPostRangeDiff.FakeGitHub qualified as FakeGitHub
import GhPostRangeDiff.Git (abbrev)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.GitHub (Ev (..))
import GhPostRangeDiff.Plan (Action (..), Plan, Shape (..), Step (..), everyRewrite, outcome)
import GhPostRangeDiff.RangeDiff qualified as RangeDiff
import GhPostRangeDiff.Render (format)
import GhPostRangeDiff.Repo
import GhPostRangeDiff.Run (Reported (..), run)
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (conjoin, counterexample, tabulate, (===))

-- | One version of the branch pushed: what the push was, what it should get
-- reported with, and what reporting on it came to.
data Push = Push
  { pshEv :: Ev,
    -- | what the base under the branch did to get here, which is only good for
    -- saying what a scenario covered
    pshBaseMoved :: String,
    pshRangeDiff :: [RangeDiff.Commit],
    pshReported :: Reported
  }

-- | Push the version of the branch a rewrite left behind: the commit it forks
-- off onto the branch the pull request targets, its tip onto the branch under
-- review. Then report on it where the action reports on it: against the pull
-- request as GitHub has it so far, in a repo of its own that has fetched
-- nothing yet, with the repo the pull request is on as its origin. Whatever the
-- tool needs to diff, it has to go and get.
push :: PullRequest -> Rewrite -> IO Push
push pr rw = do
  let Range base head' = rwNow rw
  pushed <- FakeGitHub.push pr base head'
  ev <- case pushed of
    Nothing -> fail "the branch came out as it already was, so nothing was pushed"
    Just ev -> pure ev
  moved <- baseMovement (rgBase (rwWas rw)) base
  reported <-
    withSystemTempDirectory "gh-post-range-diff-checkout" $ \dir -> withCurrentDirectory dir $ do
      _ <- git ["init", "-q"]
      _ <- git ["remote", "add", "origin", FakeGitHub.origin pr]
      -- The scenario knows its commits by their full shas, so the range-diff
      -- taken here has to name them the same way the scenario's repo does.
      _ <- git ["config", "core.abbrev", "no"]
      run (FakeGitHub.handle pr) (evBefore ev) (evAfter ev)
  pure (Push ev moved (rwRangeDiff rw) reported)
  where
    git = Git.sh "git"

-- | What a push should get reported with: a header naming both ends of it, and
-- the range-diff of the rewrite it pushed.
expected :: Push -> String
expected p =
  "### Range-diff for push "
    ++ abbrev (evBefore (pshEv p))
    ++ " → "
    ++ abbrev (evAfter (pshEv p))
    ++ "\n\n"
    ++ format (pshRangeDiff p)

-- | What a comment says, which is everything below whatever the tool hid at the
-- top of it. An HTML comment renders as nothing, so what one holds is no part
-- of what the comment says.
said :: String -> String
said = intercalate "\n" . dropWhile ("<!--" `isPrefixOf`) . lines

-- | Whether the tool can tell, for every rewrite in a plan, where the version
-- before it began.
unambiguous :: Plan -> Bool
unambiguous = everyRewrite (\sh s -> not (ambiguous sh s))

-- | Whether a rewrite leaves the tool no way of telling where the version
-- before it began.
--
-- Say a version commits @A@ and then @B@ under the fork point, and the rewrite
-- drops @B@: the fork point sinks onto @A@, which the older version had under
-- its own fork point too. @A@ is an ancestor of the older head as well, and the
-- newer of the two bases the pull request records, so the tool takes it as the
-- base of both versions. The older version then reads as starting at @A@, with
-- @B@ a commit of the branch rather than one under it.
ambiguous :: Shape -> Step -> Bool
ambiguous sh s = sunk < length (shBase sh) && all untouched (take sunk (stpBase s))
  where
    sunk = length (shBase (outcome sh s))

    untouched Keep = True
    untouched _ = False

spec :: Spec
spec =
  -- Every case builds a repo and then a checkout per push, so keep the number
  -- of them small.
  modifyMaxSuccess (const 20) $
    describe "run" $
      it "posts the range-diff of every push" $
        -- Several pushes, so that the tool has several bases to tell apart and
        -- a timeline it has to read more than the last event of. It works out
        -- from that timeline where the old version began, so a scenario that
        -- leaves it no way of telling is one this can't hold it to.
        withRepo (2, 4) unambiguous $ \repo -> do
          pr <- newPullRequest repo
          -- Each push is reported on as it happens, so the tool only ever sees
          -- the timeline GitHub had recorded by then.
          pushes <- evolve repo (push pr)
          posted <- FakeGitHub.comments pr
          pure
            . tabulate "base" (map pshBaseMoved pushes)
            $ conjoin
              [ counterexample "one comment per push, saying what that push did" (map said posted === map expected pushes),
                counterexample "every push reported" (map pshReported pushes === map (const Posted) pushes)
              ]

-- | A pull request just opened on the repo, on the version the branch was
-- initialized as. Both branches had to be pushed before there was a pull
-- request to open, so it starts out knowing where each of them is.
newPullRequest :: Repo -> IO PullRequest
newPullRequest repo = do
  Range base head' <- range repo
  FakeGitHub.newPullRequest (repoDir repo) base head'
