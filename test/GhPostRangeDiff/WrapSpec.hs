module GhPostRangeDiff.WrapSpec (spec) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GhPostRangeDiff.Wrap (wrap)
import Test.Hspec
import Test.QuickCheck

-- | A width to wrap at, and a line of an interdiff to wrap: the diff markers
-- it opens with -- none, as a header does, or one or two -- and then words and
-- the spaces and tabs between them. Most words are short, but now and then
-- one is about as long as a line, or longer.
data Case = Case Int String String
  deriving (Show)

instance Arbitrary Case where
  arbitrary = do
    w <- choose (20, 60)
    markers <- choose (0, 2) >>= (`vectorOf` elements " +-")
    n <- choose (0, 30)
    body <- concat <$> vectorOf n (frequency [(3, word w), (2, gap)])
    pure (Case w markers body)
    where
      word w = do
        n <- frequency [(9, choose (1, 8)), (1, choose (w - 8, 2 * w))]
        vectorOf n (elements "ab`")
      gap = elements [" ", "  ", "\t"]

  -- Only ever shrink what follows the markers, so that a line keeps them.
  shrink (Case w m s) = [Case w m s' | s' <- shrink s]

-- | How many columns a line takes up on GitHub, where a tab moves on to the
-- next multiple of 8.
width :: String -> Int
width = foldl advance 0
  where
    advance c '\t' = (c `div` 8 + 1) * 8
    advance c _ = c + 1

isGap :: Char -> Bool
isGap c = c == ' ' || c == '\t'

-- | What 'wrap' starts each line that carries on from the one above with,
-- after markers @m@.
continuationOf :: String -> String
continuationOf m = m ++ "\8618 "

-- | The pieces 'wrap' broke what follows markers @m@ into: each line it put
-- out, without the markers the first starts with or the continuation each
-- one after it does. Nothing if a line doesn't start with what it should.
pieces :: String -> [String] -> Maybe [String]
pieces _ [] = Nothing
pieces m (first : rest) = (:) <$> stripPrefix m first <*> traverse (stripPrefix (continuationOf m)) rest

-- | One place 'wrap' broke a line at: what the line before the break starts
-- with, what of the line wrapped it holds, and what was left of that after
-- it.
data Break = Break String String String
  deriving (Show)

-- | Every place 'wrap' broke a line at, or Nothing if not every line carries
-- on from the one above.
breaks :: String -> String -> [String] -> Maybe [Break]
breaks m s ls = do
  ps <- pieces m ls
  let leads = m : repeat (continuationOf m)
  pure (zipWith3 Break leads (init ps) [drop n s | n <- scanl1 (+) (map length ps)])

-- | Whether a break came before the line was full: whether the next
-- character would still have fit on it.
early :: Int -> Break -> Bool
early w (Break lead piece rest) = width (lead ++ piece ++ take 1 rest) <= w

-- | Check something of every break in the line a case wraps.
forEveryBreak :: (Testable prop) => (Case -> Break -> prop) -> Case -> Property
forEveryBreak check c@(Case w m s) = case breaks m s (wrap w m s) of
  Nothing -> counterexample "not every line carries on from the one above" False
  Just bs -> conjoin [counterexample (show b) (check c b) | b <- bs]

-- | Run a property over many cases, making sure enough of them have lines
-- that fit, and lines broken both early and when full.
property' :: (Testable prop) => (Case -> prop) -> Property
property' p = checkCoverage $ forAllShrink arbitrary shrink $ \c@(Case w m s) ->
  let bs = fromMaybe [] (breaks m s (wrap w m s))
   in cover 10 (width (m ++ s) <= w) "fits" $
        cover 10 (any (early w) bs) "broken early" $
          cover 10 (not (all (early w) bs)) "broken when full" $
            cover 10 ('\t' `elem` s) "has a tab" $
              cover 10 (null m) "has no markers" $
                p c

spec :: Spec
spec = describe "wrap" $ do
  it "leaves a short line alone" $
    wrap 20 "+ " "short line" `shouldBe` ["+ short line"]

  it "breaks after the last space that fits" $
    wrap 20 "+ " "the quick brown fox jumps over"
      `shouldBe` [ "+ the quick brown ",
                   "+ ↪ fox jumps over"
                 ]

  it "carries a line with one marker on under that one" $
    wrap 20 "-" "Commits made with jj carry a change ID"
      `shouldBe` [ "-Commits made with ",
                   "-↪ jj carry a ",
                   "-↪ change ID"
                 ]

  it "carries a line with no markers on under none" $
    wrap 20 "" "diff --git a/widget.py b/widget.py"
      `shouldBe` [ "diff --git ",
                   "↪ a/widget.py ",
                   "↪ b/widget.py"
                 ]

  it "cuts a word too long for any line where the space runs out" $
    wrap 20 "-+" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJ"
      `shouldBe` [ "-+abcdefghijklmnopqr",
                   "-+↪ stuvwxyzABCDEFGH",
                   "-+↪ IJ"
                 ]

  it "fills a line before cutting a word too long for any line" $
    wrap 20 "+ " "see https://example.com/very/long/path ok"
      `shouldBe` [ "+ see https://exampl",
                   "+ ↪ e.com/very/long/",
                   "+ ↪ path ok"
                 ]

  it "breaks after a tab as well as a space" $
    wrap 20 "+ " "abcde\tfghijklmnopqrst"
      `shouldBe` [ "+ abcde\t",
                   "+ ↪ fghijklmnopqrst"
                 ]

  it "counts a tab up to the next multiple of 8" $
    wrap 20 "+ " "\tfoo\tbar baz qux"
      `shouldBe` [ "+ \tfoo\tbar ",
                   "+ ↪ baz qux"
                 ]

  it "leaves a line that fits as it is" $
    property' $ \(Case w m s) ->
      width (m ++ s) > w || wrap w m s == [m ++ s]

  it "loses nothing of the line" $
    property' $ \(Case w m s) ->
      fmap concat (pieces m (wrap w m s)) === Just s

  it "fits every line it breaks a line into" $
    property' $ \(Case w m s) ->
      counterexample (unlines (wrap w m s)) $
        all ((<= w) . width) (wrap w m s)

  it "breaks early only after a space or tab" $
    property' $
      forEveryBreak $ \(Case w _ _) b@(Break _ piece _) ->
        not (early w b) || isGap (last piece)

  it "breaks early only when the next word doesn't fit" $
    property' $
      forEveryBreak $ \(Case w _ _) (Break lead piece rest) ->
        let next = takeWhile (not . isGap) rest
         in width (lead ++ piece ++ take (length next + 1) rest) > w

  it "breaks early only before a word that fits on the next line" $
    property' $
      forEveryBreak $ \(Case w m _) b@(Break _ _ rest) ->
        not (early w b)
          || width (continuationOf m ++ takeWhile (not . isGap) rest) <= w

  it "breaks a word only if it is too long for a line of its own" $
    property' $
      forEveryBreak $ \(Case w m _) (Break _ piece rest) ->
        let start = reverse (takeWhile (not . isGap) (reverse piece))
            end = takeWhile (not . isGap) rest
         in null start
              || null end
              || width (continuationOf m ++ start ++ end) > w
