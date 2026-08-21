-- | The commit shas the tool passes around, the `git` invocations it needs, and
-- the base reconstruction on top of them.
module GhPostRangeDiff.Git
  ( CommitSha (shaText),
    commitSha,
    knownSha,
    abbrev,
    sh,
    isAncestor,
    baseFor,
    fetch,
    revParse,
    rangeDiff,
  )
where

import Control.Monad.Extra (findM)
import Data.Char (isDigit)
import Data.List.Extra (trim)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (..))
import System.Process (callProcess, readProcess, readProcessWithExitCode)

-- | A commit sha, whether abbreviated or written out in full.
newtype CommitSha = CommitSha {shaText :: String}
  deriving (Eq, Show)

-- | Read a sha, rejecting anything that isn't one. Git abbreviates to at least
-- four hex digits and never past the full forty, and prints them in lowercase.
commitSha :: String -> Maybe CommitSha
commitSha s
  | n >= 4, n <= 40, all isHex s = Just (CommitSha s)
  | otherwise = Nothing
  where
    n = length s
    isHex c = isDigit c || c `elem` ['a' .. 'f']

-- | Read a sha we already know to be one, because git or GitHub just printed it
-- as one.
knownSha :: String -> CommitSha
knownSha s = fromMaybe (error ("not a sha: " ++ s)) (commitSha s)

-- | A sha cut down to the seven digits people read commits by. Only for
-- showing: it is git's own default abbreviation, not necessarily unambiguous.
abbrev :: CommitSha -> String
abbrev = take 7 . shaText

sh :: String -> [String] -> IO String
sh cmd args = readProcess cmd args ""

git :: [String] -> IO String
git = sh "git"

isAncestor :: CommitSha -> CommitSha -> IO Bool
isAncestor a b = do
  (code, _, _) <- readProcessWithExitCode "git" ["merge-base", "--is-ancestor", shaText a, shaText b] ""
  pure (code == ExitSuccess)

revParse :: String -> IO CommitSha
revParse rev = knownSha . trim <$> git ["rev-parse", rev]

fetch :: [String] -> IO ()
fetch refs = callProcess "git" (["fetch", "--quiet", "origin"] ++ refs)

rangeDiff :: CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO String
rangeDiff oldBase oldHead newBase newHead =
  git ["range-diff", range oldBase oldHead, range newBase newHead]
  where
    range a b = shaText a ++ ".." ++ shaText b

-- The base for `head`: the most recently recorded base tip that is still an
-- ancestor of it. `cands` needs to be in chronological order (current tip last).
--
-- If none is an ancestor (e.g. the base advanced through ordinary pushes, which
-- emits no force-push events) fall back to the merge-base with the current base
-- tip: the point where `head` forked from today's base line.
baseFor :: CommitSha -> CommitSha -> [CommitSha] -> IO CommitSha
baseFor currentBase headCommit cands = do
  found <- findM (`isAncestor` headCommit) (reverse cands)
  case found of
    Just b -> pure b
    Nothing -> knownSha . trim <$> git ["merge-base", shaText currentBase, shaText headCommit]
