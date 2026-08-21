{-# OPTIONS_GHC -Wno-orphans #-}

module GhPostRangeDiff.Gen () where

import Data.Char (isPrint)
import Data.List.Extra (trimEnd)
import GhPostRangeDiff.RangeDiff (CommitMessage, commitMessage, messageText)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), frequency, listOf1, resize, suchThat)
import Test.QuickCheck.Modifiers (getPrintableString)

-- | A commit message: printable words with the occasional run of backticks,
-- which sits mid-line and so must not be read as a code fence.
--
-- Trimmed of trailing whitespace and never empty, because
-- "GhPostRangeDiff.Repo" really does commit these. @git commit -m@ cleans the subject up with
-- @--cleanup=whitespace@, which strips trailing whitespace, and refuses an
-- empty message.
instance Arbitrary CommitMessage where
  arbitrary = commitMessage <$> text `suchThat` (not . null)
    where
      text = trimEnd . unwords <$> resize 3 (listOf1 word)
      word = frequency [(4, getPrintableString <$> arbitrary), (1, pure "```")]

  -- Shrink to another message 'arbitrary' could have made. The 'String'
  -- shrinker reaches outside that set: it can pick a character that isn't
  -- printable, drop the last non-space one and leave trailing whitespace
  -- behind, or empty the message altogether. Trimming a candidate can land it
  -- back on the original, which would let shrinking loop, so those go too.
  shrink m = map commitMessage (filter usable (map trimEnd (shrink (messageText m))))
    where
      usable m' = not (null m') && all isPrint m' && m' /= messageText m
