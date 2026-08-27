-- | The commit shas a range-diff is taken over, and the ones it reports back.
module RangeDiff.CommitSha
  ( CommitSha (shaText),
    commitSha,
    knownSha,
  )
where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

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
