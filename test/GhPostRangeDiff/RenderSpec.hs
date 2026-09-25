{-# OPTIONS_GHC -Wno-orphans #-}

module GhPostRangeDiff.RenderSpec (spec) where

import Data.List (isInfixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import GhPostRangeDiff.Render (format)
import RangeDiff (Change (..), Commit (..), CommitMessage, Interdiff (interdiffText), commitMessage, interdiff, messageText)
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

-- An interdiff: arbitrary text, built from chunks so that newlines and runs
-- of backticks both turn up often.
instance Arbitrary Interdiff where
  arbitrary = interdiff . concat <$> listOf chunk
    where
      chunk =
        oneof
          [ getPrintableString <$> arbitrary,
            pure "\n",
            (`replicate` '`') <$> choose (1, 6)
          ]

  -- Shrinking the text can drop the trailing newline, which 'interdiff' then
  -- puts straight back; dropping those candidates keeps shrinking from looping
  -- on an unchanged value.
  shrink x = filter (/= x) (map interdiff (shrink (interdiffText x)))

instance Arbitrary CommitSha where
  arbitrary = vectorOf 7 (elements "0123456789abcdef") `suchThatMap` commitSha

  -- A shrunk sha can be too short, or no longer hex; 'commitSha' drops those.
  shrink = mapMaybe commitSha . shrink . shaText

instance Arbitrary Change where
  arbitrary =
    oneof
      [ pure Added,
        pure Removed,
        pure Unchanged,
        Updated <$> arbitrary
      ]

  -- A commit that carries no interdiff is the simpler counterexample, so try
  -- that before shrinking the interdiff itself. The other three are as small
  -- as a change gets.
  shrink (Updated patch) = Unchanged : map Updated (shrink patch)
  shrink _ = []

instance Arbitrary Commit where
  arbitrary = Commit <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (Commit change sha message) =
    [Commit change' sha message | change' <- shrink change]
      ++ [Commit change sha' message | sha' <- shrink sha]
      ++ [Commit change sha message' | message' <- shrink message]

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

toCommits :: [Block] -> Either String [Commit]
toCommits [] = Right []
-- A spoiler is titled by the headline of the commit it belongs to, and holds
-- that commit's interdiff.
toCommits
  ( PlainLineOfText "<details>"
      : PlainLineOfText summary
      : CodeBlock "diff" body
      : PlainLineOfText "</details>"
      : bs
    )
    | Just l <- summaryHeadline summary = do
        Header tag sha message <- parseHeader htmlStrong l
        case tag of
          TagUpdated -> (Commit (Updated (interdiff (unlines body))) sha message :) <$> toCommits bs
          _ -> Left ("spoiler on a commit that isn't updated: " ++ show l)
toCommits (PlainLineOfText l : bs) = do
  Header tag sha message <- parseHeader markdownStrong l
  let change = case tag of
        -- A bare headline for an updated commit means its interdiff was empty,
        -- so it got no spoiler.
        TagUpdated -> Updated (interdiff "")
        TagAdded -> Added
        TagRemoved -> Removed
        TagUnchanged -> Unchanged
  (Commit change sha message :) <$> toCommits bs
toCommits (CodeBlock info _ : _) = Left ("stray code block: " ++ show info)

-- | Read 'format' output back into the commits it was rendered from.
reparse :: String -> Either String [Commit]
reparse s = blocks (lines s) >>= toCommits

-- | Whether a three-backtick fence would have been closed by the interdiff
-- itself, so that `format` had to widen it.
widened :: Commit -> Bool
widened (Commit (Updated patch) _ _) = "```" `isInfixOf` interdiffText patch
widened _ = False

spec :: Spec
spec = describe "format" $
  it "renders commits that can losslessly be converted back" $
    checkCoverage $
      forAllShrink (resize 8 (listOf arbitrary)) shrink $ \commits ->
        cover 10 (any widened commits) "widened fence" $
          reparse (format commits) === Right commits
