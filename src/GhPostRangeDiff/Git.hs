-- | The commit shas the tool passes around, and the `git` invocations it needs.
module GhPostRangeDiff.Git
  ( CommitSha (shaText),
    commitSha,
    knownSha,
    abbrev,
    git,
    isAncestorOf,
    mergeBase,
    fetch,
    revParse,
    rangeDiff,
  )
where

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

-- | Run a git command in whichever repo is current, and hand back what it
-- printed on stdout.
git :: [String] -> IO String
git args = readProcess "git" args ""

-- | Does @a@ come before @b@ on the same line of history?
isAncestorOf :: CommitSha -> CommitSha -> IO Bool
isAncestorOf a b = do
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

-- | The best common ancestor of @a@ and @b@.
mergeBase :: CommitSha -> CommitSha -> IO CommitSha
mergeBase a b = knownSha . trim <$> git ["merge-base", shaText a, shaText b]
