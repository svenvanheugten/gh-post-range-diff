-- | The commit shas the tool passes around, and the repo it reads them out of,
-- behind a handle, so that it can be mocked in the tests. The one
-- implementation that talks to a real repo is 'git', which is that repo's
-- directory and the `git` invocations it takes to read it.
module GhPostRangeDiff.Git
  ( CommitSha (shaText),
    commitSha,
    knownSha,
    abbrev,
    Handle (..),
    git,
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

-- | Everything the tool does with the repo it takes its range-diffs in.
data Handle = Handle
  { -- | Bring the branch named here, and every commit listed, in from origin,
    -- and hand back the commit that branch is on. Nothing else here can be
    -- asked about a commit this has not brought in first.
    fetch :: String -> [CommitSha] -> IO CommitSha,
    -- | Does @a@ come before @b@ on the same line of history?
    isAncestorOf :: CommitSha -> CommitSha -> IO Bool,
    -- | The best common ancestor of @a@ and @b@.
    mergeBase :: CommitSha -> CommitSha -> IO CommitSha,
    -- | Line up @oldBase..oldHead@ against @newBase..newHead@, as
    -- `git range-diff` prints it.
    rangeDiff :: CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO String
  }

-- | The repo in @dir@, as the `git` CLI reaches it.
git :: FilePath -> Handle
git dir =
  Handle
    { fetch = gitFetch dir,
      isAncestorOf = gitIsAncestorOf dir,
      mergeBase = gitMergeBase dir,
      rangeDiff = gitRangeDiff dir
    }

-- Run a git command in the repo in @dir@, and hand back what it printed on
-- stdout.
command :: FilePath -> [String] -> IO String
command dir args = readProcess "git" (at dir args) ""

-- Aim a git command at the repo in @dir@. Every invocation here is aimed this
-- way, so which directory the process happens to be in never comes into it.
at :: FilePath -> [String] -> [String]
at dir args = "-C" : dir : args

-- Fetch the branch and the commits, then read back where the branch ended up.
--
-- refs/rd/base is a scratch ref holding the fetched branch tip. We need it
-- because without a destination the tip only lands in FETCH_HEAD, which this
-- same fetch also fills with every commit asked for, so we couldn't pick the
-- tip back out to rev-parse afterwards.
gitFetch :: FilePath -> String -> [CommitSha] -> IO CommitSha
gitFetch dir base commits = do
  callProcess "git" (at dir (["fetch", "--quiet", "origin", base ++ ":refs/rd/base"] ++ map shaText commits))
  revParse dir "refs/rd/base"

gitIsAncestorOf :: FilePath -> CommitSha -> CommitSha -> IO Bool
gitIsAncestorOf dir a b = do
  (code, _, _) <- readProcessWithExitCode "git" (at dir ["merge-base", "--is-ancestor", shaText a, shaText b]) ""
  pure (code == ExitSuccess)

gitMergeBase :: FilePath -> CommitSha -> CommitSha -> IO CommitSha
gitMergeBase dir a b = knownSha . trim <$> command dir ["merge-base", shaText a, shaText b]

gitRangeDiff :: FilePath -> CommitSha -> CommitSha -> CommitSha -> CommitSha -> IO String
gitRangeDiff dir oldBase oldHead newBase newHead =
  command dir ["range-diff", range oldBase oldHead, range newBase newHead]
  where
    range a b = shaText a ++ ".." ++ shaText b

-- The commit a revision names.
revParse :: FilePath -> String -> IO CommitSha
revParse dir rev = knownSha . trim <$> command dir ["rev-parse", rev]
