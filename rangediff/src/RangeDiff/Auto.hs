-- | Taking the best range-diff the repo allows.
--
-- There are two ways to take one. "RangeDiff.Jj" is the better of them: it
-- lines the two ranges up by the change ids jj writes on its commits, which
-- say outright which commit of the old range is which commit of the new one,
-- and it diffs the commits rather than their printed patches. It only works
-- where those change ids are there to read, and where there is a `jj` to read
-- them with, so where either is missing, "RangeDiff.Git" falls back on what
-- `git range-diff` can work out for itself.
module RangeDiff.Auto
  ( auto,
  )
where

import Control.Exception (IOException, try)
import Data.List (isPrefixOf)
import Data.List.Extra (splitOn)
import RangeDiff (Commit, Handle (..))
import RangeDiff.CommitSha (CommitSha, shaText)
import RangeDiff.Git qualified as Git
import RangeDiff.Jj qualified as Jj
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Range-diffs taken in the repo in @dir@, with jj where the commits allow it
-- and with git where they don't.
auto :: FilePath -> Handle
auto dir = Handle {rangeDiff = autoRangeDiff dir}

autoRangeDiff :: FilePath -> CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO [Commit]
autoRangeDiff dir oldBase oldHead newBase newHead = do
  -- The cheap question first: the change ids are read straight out of the
  -- commits, and there is no point asking after jj at all unless they are all
  -- there.
  identified <- allCarryChangeIds dir [(oldBase, oldHead), (newBase, newHead)]
  useJj <- if identified then jjInstalled else pure False
  let handle = if useJj then Jj.jj dir else Git.git dir
  rangeDiff handle oldBase oldHead newBase newHead

-- | Does every commit in any of the ranges carry a jj change id?
--
-- One commit without one is enough to rule jj out: it has no name that
-- survives a rewrite, so nothing can say what the rewrite did to it.
allCarryChangeIds :: FilePath -> [(CommitSha, CommitSha)] -> IO Bool
allCarryChangeIds dir ranges = all carriesOne . records <$> raw
  where
    -- `--header` prints each commit's object in full, NUL-terminated, which is
    -- the only way to see a header git has no format placeholder for.
    raw = Git.command dir (["rev-list", "--header"] ++ map range ranges)
    range (a, b) = shaText a ++ ".." ++ shaText b

    -- The last NUL ends the last record rather than separating it from
    -- another, so the split leaves an empty one at the end.
    records = filter (not . null) . splitOn "\0"

    -- A commit object is its headers, a blank line, and then its message,
    -- which may say anything at all -- including something that reads like a
    -- header. Only the lines above the blank one are headers. The first line
    -- is the sha `rev-list` prints the object under, not part of the object.
    carriesOne = any ("change-id " `isPrefixOf`) . takeWhile (not . null) . drop 1 . lines

-- | Is there a jj to ask?
jjInstalled :: IO Bool
jjInstalled = do
  attempt <- try (readProcessWithExitCode "jj" ["--version"] "")
  pure $ case attempt of
    Left (_ :: IOException) -> False
    Right (code, _, _) -> code == ExitSuccess
