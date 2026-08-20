-- | Generators shared between the specs.
module GhPostRangeDiff.Gen (commitMessage, shrinkCommitMessage) where

import Data.Char (isPrint)
import Data.List.Extra (trimEnd)
import Test.QuickCheck (Gen, frequency, listOf1, resize, shrink, suchThat)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Modifiers (getPrintableString)

-- | A commit message: printable words with the occasional run of backticks,
-- which sits mid-line and so must not be read as a code fence.
--
-- Trimmed of trailing whitespace and never empty, because RangeDiffSpec
-- really does commit these. @git commit -m@ cleans the subject up with
-- @--cleanup=whitespace@, which strips trailing whitespace, and refuses an
-- empty message.
commitMessage :: Gen String
commitMessage = (trimEnd . unwords <$> resize 3 (listOf1 word)) `suchThat` (not . null)
  where
    word = frequency [(4, getPrintableString <$> arbitrary), (1, pure "```")]

-- | Shrink a commit message to another one 'commitMessage' could have made.
-- The 'String' shrinker reaches outside that set: it can pick a character that
-- isn't printable, drop the last non-space one and leave trailing whitespace
-- behind, or empty the message altogether. Trimming a candidate can land it
-- back on the original, which would let shrinking loop, so those go too.
shrinkCommitMessage :: String -> [String]
shrinkCommitMessage m = filter usable (map trimEnd (shrink m))
  where
    usable m' = not (null m') && all isPrint m' && m' /= m
