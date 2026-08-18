{-# LANGUAGE RecordWildCards #-}

-- | Tests for "GhPostRangeDiff.RangeDiff".
--
-- Rather than hand-write range-diff text (which would just encode our
-- assumptions about its format), the fixture builds a throwaway git repo, runs
-- the exact `git range-diff` invocation Run uses, and feeds the real output
-- through 'parse'. The one scenario exercises all four markers:
--
--  * @!@ updated  — a large, mostly-identical commit so range-diff pairs it
--  * @=@ unchanged
--  * @<@ removed
--  * @>@ added
module GhPostRangeDiff.RangeDiffSpec (spec) where

import Data.List.Extra (trim)
import GhPostRangeDiff.RangeDiff (Change (..), Commit (..), parse)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), proc, readCreateProcess)
import Test.Hspec

git :: FilePath -> [String] -> IO String
git dir args = readCreateProcess (proc "git" args) {cwd = Just dir} ""

-- Commit a single file with the given contents and message.
commit :: FilePath -> FilePath -> String -> String -> IO ()
commit dir name contents msg = do
  writeFile (dir ++ "/" ++ name) contents
  _ <- git dir ["add", name]
  _ <- git dir ["commit", "-q", "-m", msg]
  pure ()

-- Seven-char abbreviation of a revision, matching what range-diff prints for a
-- small repo.
short :: FilePath -> String -> IO String
short dir rev = take 7 . trim <$> git dir ["rev-parse", rev]

-- A big file so a one-word change is a small fraction of the commit, which
-- keeps range-diff pairing the two versions (marker @!@) instead of treating
-- them as an unrelated remove + add.
big :: String -> String
big lastLine = unlines (["l" ++ show n | n <- [1 :: Int .. 8]] ++ [lastLine])

-- The parsed range-diff plus the abbreviated shas the parser should pick: the
-- new side for =/!/>, the old side for <.
data Fixture = Fixture
  { commits :: [Commit],
    newBig, newKeep, newFresh, oldGone :: String
  }

buildFixture :: IO Fixture
buildFixture = withSystemTempDirectory "gh-post-range-diff" $ \dir -> do
  _ <- git dir ["init", "-q"]
  _ <- git dir ["config", "user.email", "t@t"]
  _ <- git dir ["config", "user.name", "t"]

  commit dir "base.txt" "shared\n" "base"
  base <- trim <$> git dir ["rev-parse", "HEAD"]

  _ <- git dir ["checkout", "-q", "-b", "old"]
  commit dir "big.txt" (big "```OLD") "big feature" -- becomes UPDATED
  commit dir "keep.txt" "x\n" "add keep" -- becomes UNCHANGED
  commit dir "gone.txt" "y\n" "add gone" -- becomes REMOVED
  _ <- git dir ["checkout", "-q", "-b", "new", base]
  commit dir "big.txt" (big "```NEW") "big feature" -- UPDATED (paired)
  commit dir "keep.txt" "x\n" "add keep" -- UNCHANGED
  commit dir "fresh.txt" "z\n" "add fresh" -- ADDED
  diff <- git dir ["range-diff", base ++ "..old", base ++ "..new"]

  Fixture (parse diff)
    <$> short dir "new~2"
    <*> short dir "new~1"
    <*> short dir "new"
    <*> short dir "old"

-- The interdiff git prints under the paired commit, as 'parse' should hand it
-- back: de-indented, so its own +/- sit in column 0.
interdiff :: [String]
interdiff = ["@@ big.txt (new)", " +l6", " +l7", " +l8", "-+```OLD", "++```NEW"]

spec :: Spec
spec = describe "parse" $ do
  Fixture {..} <- runIO buildFixture

  it "reads a change, a sha from the surviving side, and a de-indented interdiff per commit" $
    commits
      `shouldBe` [ Commit (Updated interdiff) newBig "big feature",
                   Commit Unchanged newKeep "add keep",
                   Commit Removed oldGone "add gone",
                   Commit Added newFresh "add fresh"
                 ]
