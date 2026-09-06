-- | Working out where the two versions of a branch fork off, over a base
-- branch that keeps moving under them.
--
-- A scenario here is a list of versions of the repo: the base branch as it
-- stands, and the one commit that is the feature branch, sitting somewhere
-- along it. Each version is pushed in turn, and the answer is known ahead of
-- the asking — a version forks off the base branch wherever its commit sits —
-- so what is being asked is whether the tool can work that back out from the
-- pull request's timeline, however far the base branch has moved since.
--
-- There are two ways a base branch moves, and a property each. It is rewritten:
-- commits are taken out of it, put on the end of it, or it is reset to a version
-- it had, and the branch is rebased onto its tip every time. Or it is a trunk:
-- commits only ever land on the end of it, and the branch forks off somewhere
-- along it and catches up in its own time.
module GhPostRangeDiff.BasesSpec (spec) where

import Data.List (isPrefixOf, tails)
import GhPostRangeDiff.Bases (Bases (..), bases)
import GhPostRangeDiff.FakeGit (Change (..), ChangeNo (..))
import GhPostRangeDiff.FakeGit qualified as FakeGit
import GhPostRangeDiff.FakeGitHub qualified as FakeGitHub
import GhPostRangeDiff.Git (abbrev)
import GhPostRangeDiff.GitHub (Ev (..))
import RangeDiff.CommitSha (CommitSha)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

-- * A version of the repo

-- | The base branch's commits, bottom-most first, and which one of them the
-- feature commit sits on.
data Version = Version
  { verBase :: [ChangeNo],
    verForkedAt :: Int
  }
  deriving (Eq)

-- | A version as a counterexample reads it: the base branch by its change
-- numbers, and how far along it the feature branch forked off.
instance Show Version where
  show v = show [n | ChangeNo n <- verBase v] ++ " forked at " ++ show (verForkedAt v)

-- | The base branch as a change, its top-most commit in hand and every commit
-- under it named through its parents.
chain :: [ChangeNo] -> Change
chain = foldl1 (\parent c -> c {changeParent = Just parent}) . map (`Change` Nothing)

-- | The feature commit, on however much of the base branch it forked off.
feature :: Version -> Change
feature v = Change (ChangeNo 0) (Just (chain (take (verForkedAt v) (verBase v))))

-- | Where the base branch is, which is what the pull request targets.
baseTip :: Version -> CommitSha
baseTip = sha . chain . verBase

-- | Where the feature branch is, which is what is under review.
headTip :: Version -> CommitSha
headTip = sha . feature

-- | Where the feature branch forks off, which is the answer being asked for.
forkPoint :: Version -> CommitSha
forkPoint v = sha (chain (take (verForkedAt v) (verBase v)))

sha :: Change -> CommitSha
sha = FakeGit.cmSha . FakeGit.commit

-- | Every commit a version has: the feature commit, and the whole base branch,
-- which reaches past the fork point where the branch hasn't caught up with it.
commits :: Version -> [FakeGit.Commit]
commits v = under (feature v) ++ under (chain (verBase v))
  where
    under ch = FakeGit.commit ch : maybe [] under (changeParent ch)

-- * A base branch being rewritten

-- | How a base branch being rewritten moves from one version to the next.
-- These three are all of it, and the branch is rebased onto the tip after each.
data Op
  = -- | take the commit at this position out of the base branch
    Remove Int
  | -- | put a new commit on the end of it
    Add
  | -- | put it back at a version it has already been at
    Reset Version

-- | Start the base branch off and rewrite it, with the feature branch on its
-- tip throughout: a version of a rewritten base branch is only ever pushed
-- rebased onto it.
rewritten :: Gen [Version]
rewritten = do
  n <- movements
  width <- choose (1, 4)
  go n (ChangeNo (width + 1)) [onTip (map ChangeNo [1 .. width])]
  where
    go 0 _ history = pure (reverse history)
    go i next history@(cur : older) = do
      op <- operation cur older
      go (i - 1) (used op next) (apply next op cur : history)
    go _ _ [] = error "a scenario always has a version to move on from"

    -- Only putting a commit on the end makes a commit, so only that uses the
    -- number up; everything else moves the base branch over commits it has.
    used Add (ChangeNo n) = ChangeNo (n + 1)
    used _ next = next

-- | The branch rebased onto the tip of the base branch, which is where every
-- version of a rewritten base branch is pushed from.
onTip :: [ChangeNo] -> Version
onTip base = Version base (length base)

-- | What the base branch can be made to do next: put a commit on the end
-- always, take one out where it has one to spare, and reset back where it has
-- somewhere to go back to.
--
-- The commit at the bottom is never taken out. Every version of the repo grows
-- from the one commit the repo starts at, as a real one does, and a base branch
-- rewritten out from under that commit would share no history at all with the
-- branch under review — which is not a rewrite, it is a different repo.
operation :: Version -> [Version] -> Gen Op
operation cur older =
  frequency $
    [(3, pure Add)]
      ++ [(2, Remove <$> choose (1, length (verBase cur) - 1)) | length (verBase cur) > 1]
      ++ [(1, Reset <$> elements older) | not (null older)]

-- | Carry an operation out, with @next@ the change number to give a commit that
-- hasn't been made before.
apply :: ChangeNo -> Op -> Version -> Version
apply next op cur = case op of
  Add -> onTip (base ++ [next])
  Remove i -> onTip (take i base ++ drop (i + 1) base)
  Reset was -> was
  where
    base = verBase cur

-- * A base branch that is a trunk

-- | Grow a base branch the way a trunk grows — commits landing on the end of
-- it, and nothing ever coming off — with the feature branch forked off
-- somewhere along it.
--
-- Each version lands some commits, none of them sometimes, and moves the fork
-- point forward, by nothing at all or as far as the branch now reaches. Neither
-- ever goes back: that is what makes it a trunk, and what leaves the branch
-- forked off behind the tip until it catches up.
trunk :: Gen [Version]
trunk = do
  n <- movements
  width <- choose (1, 3)
  start <- Version (upTo width) <$> choose (1, width)
  go n start
  where
    go 0 v = pure [v]
    go i v = do
      landed <- choose (0, 2)
      let base = upTo (length (verBase v) + landed)
      caughtUpTo <- choose (verForkedAt v, length base)
      (v :) <$> go (i - 1) (Version base caughtUpTo)

    upTo n = map ChangeNo [1 .. n]

-- | How many times the base branch moves, settled before any of it is drawn.
movements :: Gen Int
movements = frequency [(6, pure 1), (3, choose (2, 4)), (1, choose (5, 8))]

-- * Movements that once went wrong

-- | Every move the base branch makes over a scenario: where it was, and where
-- it went.
moves :: [Version] -> [([ChangeNo], [ChangeNo])]
moves versions = zip bases' (drop 1 bases')
  where
    bases' = map verBase versions

-- | The base branch force-pushed back onto a commit it already held, which is
-- to say every commit it is on now, it was on before.
--
-- Catches the bug that we fixed in
-- https://github.com/svenvanheugten/gh-post-range-diff/pull/58.
rewound :: [Version] -> Bool
rewound = any (\(was, now) -> now `isPrefixOf` was && length now < length was) . moves

-- | The base branch back on a version it had left, having been somewhere else
-- in between, so that one tip of it is recorded twice over.
--
-- Catches the bug that we fixed in
-- https://github.com/svenvanheugten/gh-post-range-diff/pull/60.
returned :: [Version] -> Bool
returned versions = or [now `elem` drop 1 later | (now : later) <- tails (map verBase versions)]

-- | The base branch force-pushed, and then advanced twice.
--
-- Catches the bug that we fixed in
-- https://github.com/svenvanheugten/gh-post-range-diff/pull/56.
forcePushedThenAdvancedTwice :: [Version] -> Bool
forcePushedThenAdvancedTwice versions =
  or [advancedTwice later | Forced : later <- tails (map movement (moves versions))]
  where
    advancedTwice later = or [thenAdvanced rest | Advanced : rest <- tails later]
    thenAdvanced rest = case dropWhile (== Stayed) rest of
      Advanced : _ -> True
      _ -> False

-- | What the base branch did in one move. Only good for saying what a scenario
-- covered.
data Movement
  = -- | it is where it was
    Stayed
  | -- | it grew at its tip, which an ordinary push does, leaving no event behind
    Advanced
  | -- | it moved off the line it was on, which takes a force-push, and that
    -- leaves an event to reconstruct it from
    Forced
  deriving (Eq)

movement :: ([ChangeNo], [ChangeNo]) -> Movement
movement (was, now)
  | was == now = Stayed
  | was `isPrefixOf` now = Advanced
  | otherwise = Forced

-- * The properties

spec :: Spec
spec =
  describe "bases" $
    -- Nothing here builds a repo or runs a process, so a scenario costs almost
    -- nothing and there is no reason to be sparing with them.
    modifyMaxSuccess (const 10_000) $ do
      it "finds where each version forked off a base branch being rewritten" $
        -- Every movement that has ever gone wrong has to keep being drawn: a
        -- generator that stopped making them would leave the property passing
        -- over ground the bugs were never on, and the bugs are what it is here
        -- for.
        checkCoverage $
          forAllShrink rewritten shorter $ \versions ->
            cover 15 (rewound versions) "rewound onto a commit it held" $
              cover 10 (returned versions) "back on a version it had left" $
                cover 6 (forcePushedThenAdvancedTwice versions) "force-pushed, then advanced twice" $
                  reported versions
      it "finds where each version forked off a trunk it catches up with" $
        forAllShrink trunk shorter $ \versions ->
          tabulate "versions" [show (length versions)] (reported versions)

-- | Only ever run less of a scenario: how the base branch got where it is, is
-- the whole of what is being tested, so a version can't be lifted out of the
-- middle of it, but stopping short of one is another scenario in its own right.
shorter :: [Version] -> [[Version]]
shorter versions = [take k versions | k <- [2 .. length versions - 1]]

-- | Push every version of the branch in turn and ask, of each push, where the
-- two versions it is between fork off.
reported :: [Version] -> Property
reported [] = property Discard
reported (first : rest) = ioProperty $ do
  origin <- FakeGit.new
  FakeGit.store origin (concatMap commits (first : rest))
  pr <- FakeGitHub.newPullRequest origin (baseTip first) (headTip first)
  conjoin <$> traverse (push origin pr) (zip (first : rest) rest)

-- | Push the version of the branch, then work out where it and the version it
-- replaced fork off, in a repo that has fetched nothing yet: whatever that
-- takes, it has to go and get.
push :: FakeGit.Repo -> FakeGitHub.PullRequest -> (Version, Version) -> IO Property
push origin pr (was, now) = do
  pushed <- FakeGitHub.push pr (baseTip now) (headTip now)
  case pushed of
    -- The base branch moved and the feature branch came out where it already
    -- was, so no push happened and nothing fired. It still counts for
    -- something: the base branch has moved, and the next push is worked out
    -- through where it has been.
    Nothing -> pure (property True)
    Just ev -> do
      checkout <- FakeGit.clone origin
      found <- bases (FakeGit.handle checkout) (FakeGitHub.handle pr) (evBefore ev) (evAfter ev)
      pure $
        counterexample ("pushing " ++ show now ++ " over " ++ show was) $
          (abbrev (basesOld found), abbrev (basesNew found))
            === (abbrev (forkPoint was), abbrev (forkPoint now))
