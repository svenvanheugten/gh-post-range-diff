{-# OPTIONS_GHC -Wno-orphans #-}

module GhPostRangeDiff.RenderSpec (spec) where

import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import GhPostRangeDiff.Render (format)
import RangeDiff (Change (..), Commit (..), CommitMessage, Interdiff (interdiffShape, interdiffText), Shape (..), commitMessage, interdiff, messageText)
import RangeDiff.CommitSha (CommitSha, commitSha, shaText)
import Test.Hspec
import Test.QuickCheck

-- A commit message: printable words with the occasional run of backticks,
-- which sits mid-line and so must not be read as a code fence.
--
-- Nothing more is asked of it than that. A message goes out on the header line
-- and comes straight back off it, and 'commitMessage' has already dropped
-- everything past the first line, so whatever is left round-trips.
instance Arbitrary CommitMessage where
  arbitrary = commitMessage . unwords <$> resize 3 (listOf word)
    where
      word = frequency [(4, getPrintableString <$> arbitrary), (1, pure "```")]

  shrink m = filter (/= m) (map commitMessage (shrink (messageText m)))

instance Arbitrary Shape where
  arbitrary = elements [DiffOfPatches, DiffOfCommits]

-- An interdiff of the given shape: arbitrary text, built from chunks so that
-- newlines, runs of backticks, lines too long to fit, and the lines that start
-- a file's header or a hunk all turn up often.
--
-- It never holds a ↪, which is what 'format' marks a wrapped line with: a line
-- of its own that read like one couldn't be told apart from a wrapped line.
interdiffOf :: Shape -> Gen Interdiff
interdiffOf shape = interdiff shape . concatMap (filter (/= '\8618')) <$> listOf chunk
  where
    chunk =
      oneof
        [ getPrintableString <$> arbitrary,
          pure "\n",
          elements ["\ndiff ", "\n@@"],
          (`replicate` '`') <$> choose (1, 6),
          longLine
        ]
    longLine = do
      n <- choose (wrapWidth, 3 * wrapWidth)
      concat <$> vectorOf n (elements ["x", " ", "`", "\t", "+", "-"])

instance Arbitrary Interdiff where
  arbitrary = arbitrary >>= interdiffOf

  -- Shrinking the text can drop the trailing newline, which 'interdiff' then
  -- puts straight back; dropping those candidates keeps shrinking from looping
  -- on an unchanged value.
  shrink x = filter (/= x) (map (interdiff (interdiffShape x)) (shrink (interdiffText x)))

instance Arbitrary CommitSha where
  arbitrary = vectorOf 7 (elements "0123456789abcdef") `suchThatMap` commitSha

  -- A shrunk sha can be too short, or no longer hex; 'commitSha' drops those.
  shrink = mapMaybe commitSha . shrink . shaText

changeIn :: Shape -> Gen Change
changeIn shape =
  oneof
    [ pure Added,
      pure Removed,
      pure Unchanged,
      Updated <$> interdiffOf shape
    ]

instance Arbitrary Change where
  arbitrary = arbitrary >>= changeIn

  -- A commit that carries no interdiff is the simpler counterexample, so try
  -- that before shrinking the interdiff itself. The other three are as small
  -- as a change gets.
  shrink (Updated patch) = Unchanged : map Updated (shrink patch)
  shrink _ = []

commitIn :: Shape -> Gen Commit
commitIn shape = Commit <$> changeIn shape <*> arbitrary <*> arbitrary

instance Arbitrary Commit where
  arbitrary = arbitrary >>= commitIn

  shrink (Commit change sha message) =
    [Commit change' sha message | change' <- shrink change]
      ++ [Commit change sha' message | sha' <- shrink sha]
      ++ [Commit change sha message' | message' <- shrink message]

-- | The commits of one comment. A range-diff is taken one way, so the
-- interdiffs in it are all of one shape -- which is what reading the comment
-- back needs to be told, since nothing in it says.
data Comment = Comment Shape [Commit]
  deriving (Show)

instance Arbitrary Comment where
  arbitrary = do
    shape <- arbitrary
    Comment shape <$> resize 8 (listOf (commitIn shape))

  shrink (Comment shape commits) = Comment shape <$> shrink commits

-- | The widest 'format' shows a line of an interdiff.
wrapWidth :: Int
wrapWidth = 104

-- | How many columns a line takes up on GitHub, where a tab moves on to the
-- next multiple of 8.
width :: String -> Int
width = foldl advance 0
  where
    advance c '\t' = (c `div` 8 + 1) * 8
    advance c _ = c + 1

-- | Join every wrapped line of a diff block back onto the line it came from:
-- a line that carries on from the one above starts with what that one starts
-- with -- its diff markers, if it has any -- and then a ↪, which no line of an
-- interdiff holds of its own.
unwrap :: [String] -> [String]
unwrap (l : c : ls)
  | (m, '\8618' : ' ' : rest) <- break (== '\8618') c, m `isPrefixOf` l = unwrap ((l ++ rest) : ls)
unwrap (l : ls) = l : unwrap ls
unwrap [] = []

data Block = PlainLineOfText String | CodeBlock String [String]

isBlank :: String -> Bool
isBlank = all (`elem` " \t")

-- | Check if a line counts as a fence. If it does, return the number of backticks and
-- the text after it. CommonMark allows up to three spaces of indent on these lines:
-- https://spec.commonmark.org/0.31.2/#fenced-code-blocks
matchFence :: Int -> String -> Maybe (Int, String)
matchFence minimalNumberOfBackticks l
  | length indent <= 3, length ticks >= minimalNumberOfBackticks = Just (length ticks, rest)
  | otherwise = Nothing
  where
    (indent, l') = span (== ' ') l
    (ticks, rest) = span (== '`') l'

-- | Split Markdown into blocks.
blocks :: [String] -> Either String [Block]
blocks [] = Right []
blocks (l : ls)
  | isBlank l = blocks ls
  | Just (n, language) <- matchFence 3 l,
    '`' `notElem` language =
      case break (closes n) ls of
        (body, _ : rest) -> (CodeBlock language body :) <$> blocks rest
        (_, []) -> Left ("unterminated code fence: " ++ show l)
  | otherwise = (PlainLineOfText l :) <$> blocks ls
  where
    -- A fence closes on the first line whose own run is at least as long,
    -- followed by nothing but spaces or tabs.
    closes n = maybe False (isBlank . snd) . matchFence n

data Tag = TagAdded | TagRemoved | TagUpdated | TagUnchanged

data Header = Header Tag CommitSha CommitMessage

-- Emphasis for a headline that stands on its own, and for one inside a
-- <summary>, which sits in a raw-HTML block that Markdown doesn't reach into.
markdownStrong, htmlStrong :: String -> String
markdownStrong s = "**" ++ s ++ "**"
htmlStrong s = "<strong>" ++ s ++ "</strong>"

parseTag :: (String -> String) -> String -> Either String (Tag, String)
parseTag strong l
  | Just r <- tag ("\128994 " ++ strong "Added") = Right (TagAdded, r)
  | Just r <- tag ("\128308 " ++ strong "Removed") = Right (TagRemoved, r)
  | Just r <- tag ("\128992 " ++ strong "Updated") = Right (TagUpdated, r)
  | Just r <- tag ("\9898 " ++ strong "Unchanged") = Right (TagUnchanged, r)
  | otherwise = Left ("not a commit line: " ++ show l)
  where
    tag t = stripPrefix (t ++ " ") l

parseHeader :: (String -> String) -> String -> Either String Header
parseHeader strong l = do
  (tag, rest) <- parseTag strong l
  case break (== ' ') rest of
    (s, ' ' : message) -> case commitSha s of
      Just sha -> Right (Header tag sha (commitMessage message))
      Nothing -> Left ("commit line has no sha: " ++ show l)
    _ -> Left ("commit line has no commit message: " ++ show l)

-- | The headline out of a <summary> line, if the line is one.
summaryHeadline :: String -> Maybe String
summaryHeadline l = stripPrefix "<summary>" l >>= stripSuffix "</summary>"
  where
    stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse

toCommits :: Shape -> [Block] -> Either String [Commit]
toCommits _ [] = Right []
-- A spoiler is titled by the headline of the commit it belongs to, and holds
-- that commit's interdiff.
toCommits
  shape
  ( PlainLineOfText "<details>"
      : PlainLineOfText summary
      : CodeBlock "diff" body
      : PlainLineOfText "</details>"
      : bs
    )
    | Just l <- summaryHeadline summary = do
        Header tag sha message <- parseHeader htmlStrong l
        case tag of
          TagUpdated -> (Commit (Updated (interdiff shape (unlines (unwrap body)))) sha message :) <$> toCommits shape bs
          _ -> Left ("spoiler on a commit that isn't updated: " ++ show l)
toCommits shape (PlainLineOfText l : bs) = do
  Header tag sha message <- parseHeader markdownStrong l
  let change = case tag of
        -- A bare headline for an updated commit means its interdiff was empty,
        -- so it got no spoiler.
        TagUpdated -> Updated (interdiff shape "")
        TagAdded -> Added
        TagRemoved -> Removed
        TagUnchanged -> Unchanged
  (Commit change sha message :) <$> toCommits shape bs
toCommits _ (CodeBlock info _ : _) = Left ("stray code block: " ++ show info)

-- | Read 'format' output back into the commits it was rendered from, whose
-- interdiffs were of the given shape.
reparse :: Shape -> String -> Either String [Commit]
reparse shape s = blocks (lines s) >>= toCommits shape

-- | The lines inside the code blocks of a Markdown string. Fails if it can't
-- be split into blocks.
codeLines :: String -> [String]
codeLines s = case blocks (lines s) of
  Right bs -> concat [body | CodeBlock _ body <- bs]
  Left err -> error err

-- | Whether any line of the commit's interdiff is too long to fit, so that
-- `format` had to wrap it.
wrapped :: Commit -> Bool
wrapped (Commit (Updated patch) _ _) = any ((> wrapWidth) . width) (lines (interdiffText patch))
wrapped _ = False

-- | Whether a three-backtick fence would have been closed by the interdiff
-- itself, so that `format` had to widen it.
widened :: Commit -> Bool
widened (Commit (Updated patch) _ _) = "```" `isInfixOf` interdiffText patch
widened _ = False

-- | The lines of the diff block 'format' shows an interdiff in.
shown :: Interdiff -> [String]
shown patch = codeLines (format [Commit (Updated patch) sha (commitMessage "Add the widget")])
  where
    sha = fromMaybe (error "not a sha") (commitSha "1234567")

-- | @n@ words, of four letters each.
words' :: Int -> String
words' n = unwords (replicate n "word")

spec :: Spec
spec = describe "format" $ do
  it "renders commits that can losslessly be converted back" $
    checkCoverage $
      forAllShrink arbitrary shrink $ \(Comment shape commits) ->
        cover 10 (any widened commits) "widened fence" $
          cover 10 (any wrapped commits) "wrapped line" $
            reparse shape (format commits) === Right commits

  it "carries a wrapped line of a diff of patches on under both its markers" $
    shown (interdiff DiffOfPatches (unlines ["@@ widget.py (new)", "-+" ++ words' 30]))
      `shouldBe` ["@@ widget.py (new)", "-+" ++ words' 20 ++ " ", "-+\8618 " ++ words' 10]

  it "carries a wrapped line of a diff of commits on under its one marker" $
    shown (interdiff DiffOfCommits (unlines ["@@ -1,1 +0,0 @@", "-" ++ words' 30]))
      `shouldBe` ["@@ -1,1 +0,0 @@", "-" ++ words' 20 ++ " ", "-\8618 " ++ words' 10]

  it "carries a wrapped header of a diff of commits on under no markers" $ do
    let p = replicate 60 'x'
    shown (interdiff DiffOfCommits (unlines ["diff --git a/" ++ p ++ " b/" ++ p, "--- a/" ++ p, "+++ b/" ++ p]))
      `shouldBe` ["diff --git a/" ++ p ++ " ", "\8618 b/" ++ p, "--- a/" ++ p, "+++ b/" ++ p]

  it "wraps every line of an interdiff to fit" $
    checkCoverage $
      forAllShrink (resize 8 (listOf arbitrary)) shrink $ \commits ->
        cover 10 (any wrapped commits) "wrapped line" $
          all ((<= wrapWidth) . width) (codeLines (format commits))
