-- | Generators shared between the specs.
module GhPostRangeDiff.Gen (commitMessage, shrinkCommitMessage) where

import Data.Char (isPrint)
import Test.QuickCheck (Gen, arbitraryPrintableChar, frequency, listOf1, resize, shrink, suchThat)

-- | A commit message: printable words with the occasional run of backticks,
-- which sits mid-line and so must not be read as a code fence.
--
-- The words are separated by a single space each, and the message is never
-- empty, because RangeDiffSpec really does commit these: @git commit -m@
-- refuses an empty message, and 'GhPostRangeDiff.RangeDiff.rangeDiff' reads a
-- subject back a word at a time, so it can only be faithful to a message that
-- is spaced like this one.
commitMessage :: Gen String
commitMessage = unwords <$> resize 3 (listOf1 word)
  where
    word = frequency [(4, listOf1 wordChar), (1, pure "```")]
    wordChar = arbitraryPrintableChar `suchThat` (/= ' ')

-- | Shrink a commit message to another one 'commitMessage' could have made.
-- The 'String' shrinker reaches outside that set: it can pick a character that
-- isn't printable, empty the message altogether, or leave a run of spaces
-- behind where it dropped the only character between two of them.
shrinkCommitMessage :: String -> [String]
shrinkCommitMessage = filter usable . shrink
  where
    usable m = not (null m) && all isPrint m && m == unwords (words m)
