{-# LANGUAGE RecordWildCards #-}

-- | Tests for "GhPostRangeDiff.Render".
--
-- Rather than hand-write range-diff text (which would just encode our
-- assumptions about its format), the fixture builds a throwaway git repo, runs
-- the exact `git range-diff` invocation Main uses, and feeds the real output
-- through 'format'. The one scenario exercises all four markers:
--
--  * @!@ updated  — a large, mostly-identical commit so range-diff pairs it
--  * @=@ unchanged
--  * @<@ removed
--  * @>@ added
module GhPostRangeDiff.RenderSpec (spec) where

import Data.List (intercalate)
import Data.List.Extra (trim)
import GhPostRangeDiff.Render (format)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), proc, readCreateProcess)
import Test.Hspec

-- Status emojis, as the codepoints 'format' emits.
green, red, orange, white :: String
green = "\128994" -- 🟢 added
red = "\128308" -- 🔴 removed
orange = "\128992" -- 🟠 updated
white = "\9898" -- ⚪ unchanged

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

-- The rendered comment plus the abbreviated shas the renderer should pick: the
-- new side for =/!/>, the old side for <.
data Fixture = Fixture
  { out :: String,
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

  Fixture (format diff)
    <$> short dir "new~2"
    <*> short dir "new~1"
    <*> short dir "new"
    <*> short dir "old"

spec :: Spec
spec = describe "format" $ do
  Fixture {..} <- runIO buildFixture

  it "renders a status marker and bare sha per commit, with a changed commit's interdiff shown in-place in a fence that survives backticks in the patch" $
    out
      `shouldBe` intercalate
        "\n"
        [ orange ++ " **Updated** " ++ newBig ++ " big feature",
          "",
          -- The changed line carries a ``` run, so the fence widens to
          -- four backticks to stay unbreakable.
          "````diff",
          "@@ big.txt (new)",
          " +l6",
          " +l7",
          " +l8",
          "-+```OLD",
          "++```NEW",
          "````",
          "",
          white ++ " **Unchanged** " ++ newKeep ++ " add keep",
          "",
          red ++ " **Removed** " ++ oldGone ++ " add gone",
          "",
          green ++ " **Added** " ++ newFresh ++ " add fresh"
        ]
