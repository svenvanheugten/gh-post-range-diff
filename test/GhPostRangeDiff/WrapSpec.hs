module GhPostRangeDiff.WrapSpec (spec) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GhPostRangeDiff.Wrap (wrap)
import Test.Hspec
import Test.QuickCheck

-- | A width to wrap at, and a line of an interdiff to wrap: two diff markers,
-- then words and the spaces between them. Most words are short, but
-- now and then one is about as long as a line, or longer.
data Case = Case Int String
  deriving (Show)

instance Arbitrary Case where
  arbitrary = do
    w <- choose (20, 60)
    markers <- vectorOf 2 (elements " +-")
    n <- choose (0, 30)
    body <- concat <$> vectorOf n (frequency [(3, word w), (2, gap)])
    pure (Case w (markers ++ body))
    where
      word w = do
        n <- frequency [(9, choose (1, 8)), (1, choose (w - 8, 2 * w))]
        vectorOf n (elements "ab`")
      gap = elements [" ", "  "]

  -- Only ever shrink what follows the markers, so that a line keeps them.
  shrink (Case w l) = [Case w (take 2 l ++ body) | body <- shrink (drop 2 l)]

-- | What 'wrap' starts each line that carries on from the one above with.
continuationOf :: String -> String
continuationOf l = take 2 l ++ "\8618 "

-- | The lines 'wrap' broke a line into, without the continuation each one but
-- the first starts with.
pieces :: String -> [String] -> Maybe [String]
pieces _ [] = Nothing
pieces l (first : rest) = (first :) <$> traverse (stripPrefix (continuationOf l)) rest

-- | Every place 'wrap' broke a line at: the line it put before the break, and
-- what was left of the line after it. Nothing if not every line carries on
-- from the one above.
breaks :: String -> [String] -> Maybe [(String, String)]
breaks l ls = do
  ps <- pieces l ls
  pure (zip (init ls) [drop n l | n <- scanl1 (+) (map length ps)])

-- | Whether a break came before the line was full: whether the next
-- character would still have fit on it.
early :: Int -> (String, String) -> Bool
early w (line, rest) = length (line ++ take 1 rest) <= w

-- | Check something of every break in the line a case wraps.
forEveryBreak :: (Testable prop) => (Case -> (String, String) -> prop) -> Case -> Property
forEveryBreak check c@(Case w l) = case breaks l (wrap w l) of
  Nothing -> counterexample "not every line carries on from the one above" False
  Just bs -> conjoin [counterexample (show b) (check c b) | b <- bs]

-- | Run a property over many cases, making sure enough of them have lines
-- that fit, and lines broken both early and when full.
property' :: (Testable prop) => (Case -> prop) -> Property
property' p = checkCoverage $ forAllShrink arbitrary shrink $ \c@(Case w l) ->
  let bs = fromMaybe [] (breaks l (wrap w l))
   in cover 10 (length l <= w) "fits" $
        cover 10 (any (early w) bs) "broken early" $
          cover 10 (not (all (early w) bs)) "broken when full" $
            p c

spec :: Spec
spec = describe "wrap" $ do
  it "leaves a short line alone" $
    wrap 20 "+ short line" `shouldBe` ["+ short line"]

  it "breaks after the last space that fits" $
    wrap 20 "+ the quick brown fox jumps over"
      `shouldBe` [ "+ the quick brown ",
                   "+ ↪ fox jumps over"
                 ]

  it "cuts a word too long for any line where the space runs out" $
    wrap 20 "-+abcdefghijklmnopqrstuvwxyzABCDEFGHIJ"
      `shouldBe` [ "-+abcdefghijklmnopqr",
                   "-+↪ stuvwxyzABCDEFGH",
                   "-+↪ IJ"
                 ]

  it "fills a line before cutting a word too long for any line" $
    wrap 20 "+ see https://example.com/very/long/path ok"
      `shouldBe` [ "+ see https://exampl",
                   "+ ↪ e.com/very/long/",
                   "+ ↪ path ok"
                 ]

  it "leaves a line that fits as it is" $
    property' $ \(Case w l) ->
      length l > w || wrap w l == [l]

  it "loses nothing of the line" $
    property' $ \(Case w l) ->
      fmap concat (pieces l (wrap w l)) === Just l

  it "fits every line it breaks a line into" $
    property' $ \(Case w l) ->
      counterexample (unlines (wrap w l)) $
        all ((<= w) . length) (wrap w l)

  it "breaks early only after a space" $
    property' $
      forEveryBreak $ \(Case w _) b@(line, _) ->
        not (early w b) || last line == ' '

  it "breaks early only when the next word doesn't fit" $
    property' $
      forEveryBreak $ \(Case w _) (line, rest) ->
        let next = takeWhile (/= ' ') rest
         in length (line ++ take (length next + 1) rest) > w

  it "breaks early only before a word that fits on the next line" $
    property' $
      forEveryBreak $ \(Case w l) b@(_, rest) ->
        not (early w b)
          || length (continuationOf l ++ takeWhile (/= ' ') rest) <= w

  it "breaks a word only if it is too long for a line of its own" $
    property' $
      forEveryBreak $ \(Case w l) (line, rest) ->
        let start = reverse (takeWhile (/= ' ') (reverse line))
            end = takeWhile (/= ' ') rest
         in null start
              || null end
              || length (continuationOf l ++ start ++ end) > w
