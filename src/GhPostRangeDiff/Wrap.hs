-- | Wrapping the lines of an interdiff, for a code block on GitHub, which
-- never wraps one but scrolls it sideways instead.
module GhPostRangeDiff.Wrap (wrap) where

import Data.List (dropWhileEnd)

-- | What a line that carries on from the one above starts with, straight after
-- the two diff markers it repeats.
continuation :: String
continuation = "\8618 " -- ↪

-- | Break a line of an interdiff over as many lines as it takes to fit in @w@
-- columns. Every line it is broken into starts with the same two diff
-- markers, so GitHub colours it as the line it is part of, and each one but
-- the first goes on to say that it is a 'continuation'. Breaks fall after a
-- space where there is one to break at, so that words stay whole.
wrap :: Int -> String -> [String]
wrap w l
  | length l <= w = [l]
  | otherwise = first : map (lead ++) (chop more)
  where
    lead = take 2 l ++ continuation
    (first, more) = breakAfter w lead "" l
    chop "" = []
    chop s = let (c, s') = breakAfter w lead lead s in c : chop s'

-- | Split off as much of @s@ as fits in @w@ columns behind @lead@, ending it on
-- the last space it holds. It takes at least one character, so that
-- wrapping always gets somewhere.
--
-- A word that won't fit behind @next@, the lead of the line after, will be cut
-- wherever the break falls, so breaking before it would only waste the space
-- it could have taken here. Such a word is cut where the space runs out.
breakAfter :: Int -> String -> String -> String -> (String, String)
breakAfter w next lead s
  | n >= length s = (s, "")
  | k > 0, length (next ++ takeWhile (/= ' ') (drop k s)) <= w = splitAt k s
  | otherwise = splitAt n s
  where
    n = max 1 (w - length lead)
    -- Just past the last space that fits, if there is one.
    k = length (dropWhileEnd (/= ' ') (take n s))
