{-# LANGUAGE RecordWildCards #-}

{- | Tests for "RangeDiffRenderer" and the shallow-clone handling in "Git".

Both build throwaway git repos and run the exact git plumbing Main uses, rather
than hand-writing expected git output (which would just encode our assumptions
about its format).
-}
module Main where

import Data.List (intercalate)
import Data.List.Extra (trim)
import Git (baseFor, sh, unshallowRange)
import RangeDiffRenderer (format)
import System.Directory (withCurrentDirectory)
import System.Process (CreateProcess (cwd), callProcess, proc, readCreateProcess, readProcess)
import Test.Hspec

-- Status emojis, as the codepoints 'format' emits.
green, red, orange, white :: String
green = "\128994" -- 🟢 added
red = "\128308" -- 🔴 removed
orange = "\128992" -- 🟠 updated
white = "\9898" -- ⚪ unchanged

git :: FilePath -> [String] -> IO String
git dir args = readCreateProcess (proc "git" args){cwd = Just dir} ""

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
    { out :: String
    , newBig, newKeep, newFresh, oldGone :: String
    }

buildFixture :: IO Fixture
buildFixture = do
    dir <- trim <$> readProcess "mktemp" ["-d"] ""
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

-- A throwaway bare origin plus the SHAs a push would hand to 'Main.run': the
-- base branch name, the push's before/after head SHAs, and every recorded
-- base-ref-force-push tip.
data Repo = Repo
    { origin :: FilePath
    , rBase :: String
    , rOld, rNew :: String
    , rBaseOids :: [String]
    }

rev :: FilePath -> String -> IO String
rev dir r = trim <$> git dir ["rev-parse", r]

bareOrigin :: IO FilePath
bareOrigin = do
    o <- trim <$> readProcess "mktemp" ["-d"] ""
    _ <- git o ["init", "-q", "--bare"]
    -- reconstruct fetches heads and recorded base tips by raw SHA.
    _ <- git o ["config", "uploadpack.allowAnySHA1InWant", "true"]
    pure o

workOn :: FilePath -> IO FilePath
workOn o = do
    w <- trim <$> readProcess "mktemp" ["-d"] ""
    _ <- git w ["init", "-q"]
    _ <- git w ["config", "user.email", "t@t"]
    _ <- git w ["config", "user.name", "t"]
    _ <- git w ["remote", "add", "origin", o]
    pure w

-- Clone `feat` (the PR head branch) from a bare origin; opts adds e.g. --depth=1.
cloneAt :: [String] -> FilePath -> IO FilePath
cloneAt opts o = do
    d <- trim <$> readProcess "mktemp" ["-d"] ""
    _ <- readProcess "git" (["clone", "-q"] ++ opts ++ ["--branch", "feat", "file://" ++ o, d]) ""
    pure d

-- A plain head-only force-push: both heads sit on the same, current base tip.
buildHeadForcePush :: IO Repo
buildHeadForcePush = do
    o <- bareOrigin
    w <- workOn o
    commit w "f.txt" "base\n" "base"
    _ <- git w ["branch", "-M", "main"]
    _ <- git w ["checkout", "-q", "-b", "feat"]
    commit w "f.txt" "base\nold-a\n" "a"
    commit w "f.txt" "base\nold-a\nold-b\n" "b"
    old <- rev w "HEAD"
    _ <- git w ["reset", "-q", "--hard", "main"]
    commit w "f.txt" "base\nnew-a\n" "a"
    commit w "f.txt" "base\nnew-a\nnew-b\n" "b"
    new <- rev w "HEAD"
    _ <- git w ["update-ref", "refs/heads/old", old]
    _ <- git w ["push", "-q", "origin", "main", "feat", "old"]
    pure Repo{origin = o, rBase = "main", rOld = old, rNew = new, rBaseOids = []}

-- The base branch advanced through ordinary pushes (no force-push, so no
-- recorded base tips) after the PR forked, so neither head sits on the current
-- base tip. The fast path alone must reconstruct this.
buildBaseAdvanced :: IO Repo
buildBaseAdvanced = do
    o <- bareOrigin
    w <- workOn o
    commit w "f.txt" "base0\n" "base0"
    base0 <- rev w "HEAD"
    _ <- git w ["branch", "-M", "main"]
    _ <- git w ["checkout", "-q", "-b", "feat", base0]
    commit w "g.txt" "o1\n" "o1"
    commit w "g.txt" "o1\no2\n" "o2"
    old <- rev w "HEAD"
    _ <- git w ["checkout", "-q", "-B", "feat", base0]
    commit w "g.txt" "n1\n" "n1"
    commit w "g.txt" "n1\nn2\n" "n2"
    new <- rev w "HEAD"
    _ <- git w ["checkout", "-q", "main"]
    commit w "f.txt" "base0\nadvance\n" "advance base"
    _ <- git w ["update-ref", "refs/heads/old", old]
    _ <- git w ["push", "-q", "origin", "main", "feat", "old"]
    pure Repo{origin = o, rBase = "main", rOld = old, rNew = new, rBaseOids = []}

-- A base-ref force-push: the base line r0->X->C1->C2->B_new was rewritten from X
-- to B_new, so the old head (branched at C2) resolves to base X, which sits below
-- the current merge-base and thus below a naive shallow deepening.
buildBaseForcePush :: IO Repo
buildBaseForcePush = do
    o <- bareOrigin
    w <- workOn o
    commit w "f.txt" "r0\n" "r0"
    commit w "f.txt" "r0\nx\n" "X"
    x <- rev w "HEAD"
    commit w "f.txt" "r0\nx\nc1\n" "C1"
    commit w "f.txt" "r0\nx\nc1\nc2\n" "C2"
    c2 <- rev w "HEAD"
    commit w "f.txt" "r0\nx\nc1\nc2\nbn\n" "B_new"
    bnew <- rev w "HEAD"
    _ <- git w ["branch", "-M", "main"]
    _ <- git w ["checkout", "-q", "-b", "old", c2]
    commit w "g.txt" "o1\n" "o1"
    commit w "g.txt" "o1\no2\n" "o2"
    old <- rev w "HEAD"
    _ <- git w ["checkout", "-q", "-b", "feat", bnew]
    commit w "h.txt" "n1\n" "n1"
    commit w "h.txt" "n1\nn2\n" "n2"
    new <- rev w "HEAD"
    _ <- git w ["push", "-q", "origin", "main", "feat", "old"]
    pure Repo{origin = o, rBase = "main", rOld = old, rNew = new, rBaseOids = [x, bnew]}

-- The core of 'Main.run', driven with the real 'Git' functions in `dir`: fetch
-- the base and heads, deepen if shallow, pick each base, and range-diff.
reconstruct :: FilePath -> Repo -> IO String
reconstruct dir Repo{..} = withCurrentDirectory dir $ do
    callProcess "git" $
        ["fetch", "--quiet", "origin", rBase ++ ":refs/rd/base", rOld, rNew] ++ rBaseOids
    unshallowRange rBase rOld rNew rBaseOids
    newBaseTip <- trim <$> sh "git" ["rev-parse", "refs/rd/base"]
    let cands = rBaseOids ++ [newBaseTip]
    b1 <- baseFor newBaseTip rOld cands
    b2 <- baseFor newBaseTip rNew cands
    sh "git" ["range-diff", b1 ++ ".." ++ rOld, b2 ++ ".." ++ rNew]

isShallow :: FilePath -> IO Bool
isShallow dir = (== "true") . trim <$> git dir ["rev-parse", "--is-shallow-repository"]

main :: IO ()
main = hspec $ do
    describe "RangeDiffRenderer.format" $ do
        Fixture{..} <- runIO buildFixture

        it "renders a status marker and bare sha per commit, with a changed commit's interdiff shown in-place in a fence that survives backticks in the patch" $
            out
                `shouldBe` intercalate
                    "\n"
                    [ orange ++ " **Updated** " ++ newBig ++ " big feature"
                    , ""
                    , -- The changed line carries a ``` run, so the fence widens to
                      -- four backticks to stay unbreakable.
                      "````diff"
                    , "@@ big.txt (new)"
                    , " +l6"
                    , " +l7"
                    , " +l8"
                    , "-+```OLD"
                    , "++```NEW"
                    , "````"
                    , ""
                    , white ++ " **Unchanged** " ++ newKeep ++ " add keep"
                    , ""
                    , red ++ " **Removed** " ++ oldGone ++ " add gone"
                    , ""
                    , green ++ " **Added** " ++ newFresh ++ " add fresh"
                    ]

    describe "Git.unshallowRange" $ do
        -- Every shape must reconstruct from a --depth=1 clone byte-for-byte like a
        -- full clone. Ordinary shapes must also stay shallow: the fast path only
        -- ever pulls the pushed commits, never the whole history. The one exception
        -- is a head stranded below the merge-base by a base-ref force-push, which
        -- unshallowRange handles by --unshallow-ing that head (expectShallow False).
        let reconstructsLikeFull expectShallow build = do
                repo <- build
                full <- cloneAt [] (origin repo)
                shallow <- cloneAt ["--depth=1"] (origin repo)
                fullOut <- reconstruct full repo
                shallowOut <- reconstruct shallow repo
                shallowOut `shouldBe` fullOut
                isShallow shallow `shouldReturn` expectShallow

        it "reconstructs a head-only force-push and stays shallow" $
            reconstructsLikeFull True buildHeadForcePush

        it "reconstructs a PR whose base advanced through ordinary pushes and stays shallow" $
            reconstructsLikeFull True buildBaseAdvanced

        it "reconstructs a base-ref force-push with the old head on a deeper base, unshallowing" $
            reconstructsLikeFull False buildBaseForcePush
