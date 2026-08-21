module GhPostRangeDiff.RangeDiffSpec (spec) where

import Data.List (sortOn)
import Data.Maybe (isJust, mapMaybe)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.RangeDiff qualified as RangeDiff
import GhPostRangeDiff.Scenario
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (counterexample, forAllShrink, ioProperty, tabulate, (===))

-- | What became of one change between two versions of the branch. 'Nothing'
-- where neither version carries it, so it has no range-diff entry at all.
data Fate
  = -- | committed identically by both versions (@=@)
    Kept
  | -- | same message and file, different last line (@!@), which the two carry
    Rewritten String String
  | -- | committed by the old version only (@<@)
    Dropped
  | -- | committed by the new version only (@>@)
    Introduced

fate :: Version -> Version -> Change -> Maybe Fate
fate old new ch = case (stateAt old ch, stateAt new ch) of
  (Nothing, Nothing) -> Nothing
  (Just a, Just b)
    | a == b -> Just Kept
    | otherwise -> Just (Rewritten a b)
  (Just _, Nothing) -> Just Dropped
  (Nothing, Just _) -> Just Introduced

-- | How that fate should be reported for the @n@th change, and which of the two
-- versions still has a commit to point at.
reported :: Version -> Version -> ChangeNo -> Fate -> (RangeDiff.Change, Version)
reported old new n f = case f of
  Kept -> (RangeDiff.Unchanged, new)
  Rewritten a b -> (RangeDiff.Updated (expectedInterdiff n a b), new)
  -- A dropped commit only exists on the old side.
  Dropped -> (RangeDiff.Removed, old)
  Introduced -> (RangeDiff.Added, new)

-- | The marker git prints for a change between two versions, where it prints
-- one. Only good for saying what a scenario covered; 'expectedCommits' is what
-- the specs compare against.
marker :: Version -> Version -> Change -> Maybe String
marker old new ch = pick <$> fate old new ch
  where
    pick f = case f of
      Kept -> "="
      Rewritten _ _ -> "!"
      Dropped -> "<"
      Introduced -> ">"

-- | The commits 'RangeDiff.rangeDiff' should report between two versions of the
-- branch.
expectedCommits :: Repo -> Scenario -> Version -> Version -> [RangeDiff.Commit]
expectedCommits repo sc old new =
  [ RangeDiff.Commit c (shaOf repo side n) (chMessage ch)
  | (n, ch) <- snd (regions sc),
    Just f <- [fate old new ch],
    let (c, side) = reported old new n f
  ]

-- | A range as the two revisions `git range-diff` takes it as.
revs :: Range -> (String, String)
revs (Range base tip) = (RangeDiff.shaText base, RangeDiff.shaText tip)

-- | The push a scenario of two versions describes: the branch as it was, and as
-- it is now.
oldV, newV :: Version
(oldV, newV) = (Version 0, Version 1)

-- | Sort commits, so two lists holding the same ones compare equal whatever
-- order they arrived in. Every commit shows differently from every other, so
-- the shown form is as good a sort key as any.
canonical :: [RangeDiff.Commit] -> [RangeDiff.Commit]
canonical = sortOn show

spec :: Spec
spec = describe "rangeDiff" $
  -- Every case shells out to build a real repo, so keep the number of them small.
  modifyMaxSuccess (const 20) $
    it "reads a change, a sha from the surviving side and a de-indented interdiff per commit" $
      -- A range-diff always only compares two scenarios.
      forAllShrink (scenarioOf 2) shrinkScenario $ \sc -> ioProperty $ withRepo sc $ \repo -> do
        let changes = map snd (snd (regions sc))
            (oldBase, oldHead) = revs (rangeOf repo oldV)
            (newBase, newHead) = revs (rangeOf repo newV)
        commits <- RangeDiff.rangeDiff oldBase oldHead newBase newHead
        -- The unparsed range-diff is the first thing you want to look at when the
        -- model of git's output turns out to be the thing that is wrong.
        diff <- Git.rangeDiff oldBase oldHead newBase newHead
        pure
          . tabulate "markers" (mapMaybe (marker oldV newV) changes)
          . tabulate "base" [if oldBase == newBase then "shared" else "moved"]
          . tabulate "commits on the larger side" [bucket (widest changes)]
          . counterexample ("range-diff:\n" ++ diff)
          $ canonical commits === canonical (expectedCommits repo sc oldV newV)
  where
    widest changes = maximum [length (filter (isJust . stateAt v) changes) | v <- [oldV, newV]]
    bucket n = if n >= 10 then "ten or more" else "fewer than ten"
