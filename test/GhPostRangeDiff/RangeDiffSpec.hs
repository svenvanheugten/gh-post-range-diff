{-# LANGUAGE RecordWildCards #-}

-- | Tests for "GhPostRangeDiff.RangeDiff".
--
-- Rather than hand-write range-diff text (which would just encode our
-- assumptions about its format), the fixture builds a throwaway git repo and
-- runs 'rangeDiff' over it, so the exact `git range-diff` invocation Run uses
-- is the one under test. The one scenario exercises all four markers:
--
--  * @!@ updated  — a large, mostly-identical commit so range-diff pairs it
--  * @=@ unchanged
--  * @<@ removed
--  * @>@ added
module GhPostRangeDiff.RangeDiffSpec (spec) where

import GhPostRangeDiff.Git (revParse, sh)
import GhPostRangeDiff.RangeDiff (Change (..), Commit (..), rangeDiff)
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

git :: [String] -> IO String
git = sh "git"

-- Commit a single file with the given contents and message.
commit :: FilePath -> String -> String -> IO ()
commit name contents msg = do
  writeFile name contents
  _ <- git ["add", name]
  _ <- git ["commit", "-q", "-m", msg]
  pure ()

-- Seven-char abbreviation of a revision, matching what range-diff prints for a
-- small repo.
short :: String -> IO String
short rev = take 7 <$> revParse rev

-- A big file so a one-word change is a small fraction of the commit, which
-- keeps range-diff pairing the two versions (marker @!@) instead of treating
-- them as an unrelated remove + add.
big :: String -> String
big lastLine = unlines (["l" ++ show n | n <- [1 :: Int .. 8]] ++ [lastLine])

-- The range-diff of the built repo plus the abbreviated shas the parser should
-- pick: the new side for =/!/>, the old side for <.
data Fixture = Fixture
  { commits :: [Commit],
    newBig, newKeep, newFresh, oldGone :: String
  }

-- 'rangeDiff' shells out to git in the current directory, so the whole fixture
-- runs with the throwaway repo as cwd.
buildFixture :: IO Fixture
buildFixture = withSystemTempDirectory "gh-post-range-diff" $ \dir -> withCurrentDirectory dir $ do
  _ <- git ["init", "-q"]
  _ <- git ["config", "user.email", "t@t"]
  _ <- git ["config", "user.name", "t"]

  commit "base.txt" "shared\n" "base"
  base <- revParse "HEAD"

  _ <- git ["checkout", "-q", "-b", "old"]
  commit "big.txt" (big "```OLD") "big feature" -- becomes UPDATED
  commit "keep.txt" "x\n" "add keep" -- becomes UNCHANGED
  commit "gone.txt" "y\n" "add gone" -- becomes REMOVED
  _ <- git ["checkout", "-q", "-b", "new", base]
  commit "big.txt" (big "```NEW") "big feature" -- UPDATED (paired)
  commit "keep.txt" "x\n" "add keep" -- UNCHANGED
  commit "fresh.txt" "z\n" "add fresh" -- ADDED
  Fixture
    <$> rangeDiff base "old" base "new"
    <*> short "new~2"
    <*> short "new~1"
    <*> short "new"
    <*> short "old"

-- The interdiff git prints under the paired commit, as 'rangeDiff' should hand
-- it back: de-indented, so its own +/- sit in column 0.
interdiff :: [String]
interdiff = ["@@ big.txt (new)", " +l6", " +l7", " +l8", "-+```OLD", "++```NEW"]

spec :: Spec
spec = describe "rangeDiff" $ do
  Fixture {..} <- runIO buildFixture

  it "reads a change, a sha from the surviving side, and a de-indented interdiff per commit" $
    commits
      `shouldBe` [ Commit (Updated interdiff) newBig "big feature",
                   Commit Unchanged newKeep "add keep",
                   Commit Removed oldGone "add gone",
                   Commit Added newFresh "add fresh"
                 ]
