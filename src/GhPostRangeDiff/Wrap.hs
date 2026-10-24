-- | Wrapping the lines of an interdiff, for a code block on GitHub, which
-- never wraps one but scrolls it sideways instead.
module GhPostRangeDiff.Wrap (wrap) where

import Data.List (dropWhileEnd)

-- | What a line that carries on from the one above starts with, straight after
-- the two diff markers it repeats.
continuation :: String
continuation = "\8618 " -- ↪

-- | GitHub's tab size: a tab moves on to the next multiple of this many
-- columns, unless a reader has set a tab size of their own.
tabStop :: Int
tabStop = 8

-- | The column a character leaves the cursor at, when it starts at column @c@.
advance :: Int -> Char -> Int
advance c '\t' = (c `div` tabStop + 1) * tabStop
advance c _ = c + 1

-- | How many columns a line takes up on GitHub.
width :: String -> Int
width = foldl' advance 0

-- | Break a line of an interdiff over as many lines as it takes to fit in @w@
-- columns. Every line it is broken into starts with the same two diff
-- markers, so GitHub colours it as the line it is part of, and each one but
-- the first goes on to say that it is a 'continuation'. Breaks fall after a
-- space or tab where there is one to break at, so that words stay whole.
wrap :: Int -> String -> [String]
wrap w l
  | width l <= w = [l]
  | otherwise = first : map (lead ++) (chop more)
  where
    lead = take 2 l ++ continuation
    (first, more) = breakAfter w lead "" l
    chop "" = []
    chop s = let (c, s') = breakAfter w lead lead s in c : chop s'

-- | Split off as much of @s@ as fits in @w@ columns behind @lead@, ending it on
-- the last space or tab it holds. It takes at least one character, so that
-- wrapping always gets somewhere.
--
-- A word that won't fit behind @next@, the lead of the line after, will be cut
-- wherever the break falls, so breaking before it would only waste the space
-- it could have taken here. Such a word is cut where the space runs out.
--
-- A tab is measured where it ends up: in the piece, behind @lead@, rather than
-- where it was in the line the piece came from.
breakAfter :: Int -> String -> String -> String -> (String, String)
breakAfter w next lead s
  | n >= length s = (s, "")
  | k > 0, width (next ++ takeWhile (not . isGap) (drop k s)) <= w = splitAt k s
  | otherwise = splitAt n s
  where
    -- The column each character of @s@ leaves the cursor at, behind @lead@.
    ends = drop 1 (scanl advance (width lead) s)
    n = max 1 (length (takeWhile (<= w) ends))
    -- Just past the last space or tab that fits, if there is one.
    k = length (dropWhileEnd (not . isGap) (take n s))
    isGap c = c == ' ' || c == '\t'
