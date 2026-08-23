-- | Reading the range-diff of one rewrite of a branch: build a repo, rewrite
-- the branch in it once, and check that what `git range-diff` is read as says
-- what that rewrite did.
module GhPostRangeDiff.RangeDiffSpec (spec) where

import Data.List (isPrefixOf)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.RangeDiff (Change (..), Commit (..), interdiffText)
import GhPostRangeDiff.RangeDiff qualified as RangeDiff
import GhPostRangeDiff.Repo
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Property, conjoin, counterexample, tabulate, (===))

-- | The marker git prints a commit under. Only good for saying what a rewrite
-- covered.
marker :: Change -> String
marker Unchanged = "="
marker (Updated _) = "!"
marker Removed = "<"
marker Added = ">"

-- | What a rewrite rewrote, where it rewrote a commit at all: the hunks its
-- interdiff came out with say whether the message was reworded, the file
-- amended, or both. Also only good for saying what a rewrite covered.
rewrites :: Commit -> [String]
rewrites (Commit (Updated patch) _ _) =
  ["reworded" | metadata `elem` hunks] ++ ["amended" | any (/= metadata) hunks]
  where
    hunks = filter ("@@ " `isPrefixOf`) (lines (interdiffText patch))
    metadata = "@@ Metadata"
rewrites _ = []

spec :: Spec
spec = describe "rangeDiff" $
  -- Every case shells out to build a real repo, so keep the number of them small.
  modifyMaxSuccess (const 20) $
    it "reads a change, a sha from the surviving side and a de-indented interdiff per commit" $
      -- The bases a range-diff is taken over come from the scenario, which
      -- knows them, so there is no plan this can't be asked of.
      withRepo (1, 1) (const True) $
        \repo -> conjoin <$> evolve repo readBack

-- | What one rewrite of the branch reads back as, which should be what the
-- scenario says it did to every commit, and nothing else.
readBack :: Rewrite -> IO Property
readBack rw = do
  commits <- RangeDiff.rangeDiff oldBase oldHead newBase newHead
  -- The unparsed range-diff is the first thing you want to look at when the
  -- model of git's output turns out to be the thing that is wrong.
  diff <- Git.rangeDiff oldBase oldHead newBase newHead
  base <- baseMovement oldBase newBase
  pure
    . tabulate "markers" (map (marker . cmChange) want)
    . tabulate "rewrites" (concatMap rewrites want)
    . tabulate "base" [base]
    . tabulate "commits on the larger side" [bucket (length want)]
    . counterexample ("range-diff:\n" ++ diff)
    $ commits === want
  where
    Range oldBase oldHead = rwWas rw
    Range newBase newHead = rwNow rw
    want = rwRangeDiff rw
    bucket n = if n >= 10 then "ten or more" else "fewer than ten"
