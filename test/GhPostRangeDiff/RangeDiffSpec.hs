{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module GhPostRangeDiff.RangeDiffSpec (spec) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import GhPostRangeDiff.Gen qualified as Gen
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.RangeDiff qualified as RangeDiff
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    choose,
    counterexample,
    forAllShrink,
    frequency,
    getPrintableString,
    ioProperty,
    oneof,
    shrinkList,
    suchThat,
    tabulate,
    vectorOf,
    (===),
  )

-- * The scenario

-- | What happens to one commit between the old and the new branch. An 'Updated'
-- commit carries the last line it ends up with, which is the one line of the
-- interdiff whose content we get to choose.
data Change
  = -- | committed identically on both sides (@=@)
    Unchanged
  | -- | same message and file, different last line (@!@)
    Updated String
  | -- | committed on the old side only (@<@)
    Removed
  | -- | committed on the new side only (@>@)
    Added
  deriving (Show)

instance Arbitrary Change where
  arbitrary = oneof [pure Unchanged, Updated <$> payload, pure Removed, pure Added]
    where
      payload = (getPrintableString <$> arbitrary) `suchThat` (/= oldLine)

  shrink = \case
    Unchanged -> []
    Updated p -> Unchanged : [Updated p' | p' <- shrinkList (const []) p, p' /= oldLine]
    Removed -> [Unchanged]
    Added -> [Unchanged]

-- | One commit's worth of scenario: what happens to it between the branches, and
-- the message it is committed with on whichever sides it appears.
data Step = Step
  { stChange :: Change,
    stMessage :: String
  }
  deriving (Show)

instance Arbitrary Step where
  arbitrary = Step <$> arbitrary <*> Gen.commitMessage

  shrink (Step change message) =
    [Step change' message | change' <- shrink change]
      ++ [Step change message' | message' <- Gen.shrinkCommitMessage message]

-- | Does the scenario leave at least one commit on each branch? `git range-diff`
-- refuses an empty commit range, so it has to.
bothSidesNonEmpty :: [Step] -> Bool
bothSidesNonEmpty ss = any (onSide Old . stChange) ss && any (onSide New . stChange) ss

-- | Which side of the fork we are talking about.
data Side = Old | New

-- | Does the change put a commit on this side of the fork?
onSide :: Side -> Change -> Bool
onSide Old = \case
  Added -> False
  _ -> True
onSide New = \case
  Removed -> False
  _ -> True

-- | The file the @n@th change commits to. One file each, so unrelated commits
-- can never be mistaken for one another.
file :: Int -> FilePath
file n = "f" ++ show n ++ ".txt"

-- | The last line of an 'Updated' commit's file on the old side.
oldLine :: String
oldLine = "~old~"

-- * Building the repo

git :: [String] -> IO String
git = Git.sh "git"

-- | Commit a single file with the given contents and message.
commit :: FilePath -> String -> String -> IO ()
commit name contents msg = do
  writeFile name contents
  _ <- git ["add", name]
  _ <- git ["commit", "-q", "-m", msg]
  pure ()

-- | A big file, so a one-line change is a small fraction of the commit, which
-- keeps range-diff pairing the two versions (marker @!@) instead of treating them
-- as an unrelated removal plus addition.
big :: String -> String
big lastLine = unlines (["l" ++ show n | n <- [1 :: Int .. 8]] ++ [lastLine])

-- | Commit the @n@th step on one side of the fork, if it belongs there.
commitStep :: Side -> Int -> Step -> IO ()
commitStep side n (Step change message)
  | not (onSide side change) = pure ()
  | otherwise = commit (file n) contents message
  where
    contents = case (side, change) of
      (_, Unchanged) -> "x\n"
      (Old, Updated _) -> big oldLine
      (New, Updated p) -> big p
      (_, Removed) -> "y\n"
      (_, Added) -> "z\n"

-- | What 'rangeDiff' made of a scenario, and the sha of each commit on each side.
data Fixture = Fixture
  { fixCommits :: [RangeDiff.Commit],
    -- | the same range-diff as text, kept only so a failure can show it
    fixDiff :: String,
    fixOldShas, fixNewShas :: [(Int, String)]
  }

-- | 'rangeDiff' shells out to git in the current directory, so the whole fixture
-- runs with the throwaway repo as cwd.
buildFixture :: [Step] -> IO Fixture
buildFixture steps = withSystemTempDirectory "gh-post-range-diff" $ \dir -> withCurrentDirectory dir $ do
  _ <- git ["init", "-q"]
  _ <- git ["config", "user.email", "t@t"]
  _ <- git ["config", "user.name", "t"]

  commit "base.txt" "shared\n" "base"
  base <- Git.revParse "HEAD"

  let commitAll side = mapM_ (uncurry (commitStep side)) (zip [1 ..] steps)
  _ <- git ["checkout", "-q", "-b", "old"]
  commitAll Old
  _ <- git ["checkout", "-q", "-b", "new", base]
  commitAll New

  commits <- mapM expandSha =<< RangeDiff.rangeDiff base "old" base "new"
  -- The unparsed range-diff is the first thing you want to look at when the model
  -- of git's output below turns out to be the thing that is wrong.
  diff <- Git.rangeDiff base "old" base "new"
  Fixture commits diff <$> sideShas base Old steps <*> sideShas base New steps

-- | Replace the abbreviated sha git printed with the full sha it stands for, so
-- it can be compared with the one the scenario expects. How far git abbreviates
-- is its own business; `rev-parse` undoes exactly that, and fails on an
-- abbreviation that names no commit.
expandSha :: RangeDiff.Commit -> IO RangeDiff.Commit
expandSha c = do
  full <- Git.revParse (RangeDiff.shaText (RangeDiff.cmCommitSha c))
  pure c {RangeDiff.cmCommitSha = knownSha full}

-- | Read a sha we know to be one.
knownSha :: String -> RangeDiff.CommitSha
knownSha s = fromMaybe (error ("not a sha: " ++ s)) (RangeDiff.commitSha s)

-- | Which change ended up as which commit on a branch. `rev-list` is newest
-- first, so it lines up with the scenario once reversed.
sideShas :: String -> Side -> [Step] -> IO [(Int, String)]
sideShas base side steps = do
  shas <- reverse . lines <$> git ["rev-list", base ++ ".." ++ ref]
  pure (zip [n | (n, s) <- zip [1 ..] steps, onSide side (stChange s)] shas)
  where
    ref = case side of
      Old -> "old"
      New -> "new"

-- * What we expect to get back

-- | The interdiff git prints for an 'Updated' commit, as 'rangeDiff' should hand
-- it back: de-indented, so its own +/- sit in column 0. It holds the rewritten
-- hunk header, the three lines of context a default @-U3@ diff leaves before the
-- change (the tail of 'big'), then the last line as the old patch had it and as
-- the new one has it.
interdiffLines :: Int -> String -> [String]
interdiffLines n p =
  ["@@ " ++ file n ++ " (new)"]
    ++ [" +l" ++ show k | k <- [6 .. 8 :: Int]]
    ++ ["-+" ++ oldLine, "++" ++ p]

-- | The commit 'rangeDiff' should report for every step. Which order git reports
-- them in is git's business, so this is only ever compared as a set; see 'canonical'.
expected :: [(Int, String)] -> [(Int, String)] -> [Step] -> [RangeDiff.Commit]
expected oldShas newShas steps = [entry n st | (n, st) <- zip [1 ..] steps]
  where
    entry n st = RangeDiff.Commit change (sha shas n) (stMessage st)
      where
        (change, shas) = case stChange st of
          Unchanged -> (RangeDiff.Unchanged, newShas)
          Updated p -> (updated n p, newShas)
          -- A removed commit only exists on the old side.
          Removed -> (RangeDiff.Removed, oldShas)
          Added -> (RangeDiff.Added, newShas)
    updated n p = RangeDiff.Updated (RangeDiff.interdiff (unlines (interdiffLines n p)))
    sha shas n = knownSha (fromMaybe (error ("commit " ++ show n ++ " is not on that branch")) (lookup n shas))

-- * The property

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
      forAllShrink scenario (filter bothSidesNonEmpty . shrink) $ \steps -> ioProperty $ do
        Fixture {..} <- buildFixture steps
        pure
          . tabulate "changes" (map (marker . stChange) steps)
          . tabulate "commits on the larger side" [bucket (widest steps)]
          . counterexample ("range-diff:\n" ++ fixDiff)
          $ canonical fixCommits === canonical (expected fixOldShas fixNewShas steps)
  where
    -- A whole repo's worth of steps, one per commit. Mostly short, because every
    -- one of them really is committed, but often enough into double digits to
    -- exercise the leading space git pads a single-digit commit number out with
    -- once a range holds ten commits.
    scenario = sizedSteps `suchThat` bothSidesNonEmpty
    sizedSteps = do
      n <- frequency [(3, choose (1, 6)), (1, choose (10, 12))]
      vectorOf n arbitrary

    widest ss = maximum [length (filter (onSide side . stChange) ss) | side <- [Old, New]]
    bucket n = if n >= 10 then "ten or more" else "fewer than ten"

    marker = \case
      Unchanged -> "="
      Updated _ -> "!"
      Removed -> "<"
      Added -> ">"
