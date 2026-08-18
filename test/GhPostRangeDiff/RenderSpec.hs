module GhPostRangeDiff.RenderSpec (spec) where

import Data.List (isInfixOf, stripPrefix)
import GhPostRangeDiff.RangeDiff (Change (..), Commit (..), CommitSha, Interdiff (interdiffText), commitSha, interdiff)
import GhPostRangeDiff.Render (format)
import Test.Hspec
import Test.QuickCheck

-- | An interdiff: arbitrary text, built from chunks so that newlines and runs
-- of backticks both turn up often.
genInterdiff :: Gen Interdiff
genInterdiff = interdiff . concat <$> listOf chunk
  where
    chunk =
      oneof
        [ getPrintableString <$> arbitrary,
          pure "\n",
          (`replicate` '`') <$> choose (1, 6)
        ]

genChange :: Gen Change
genChange =
  oneof
    [ pure Added,
      pure Removed,
      pure Unchanged,
      Updated <$> genInterdiff
    ]

genCommitSha :: Gen CommitSha
genCommitSha = vectorOf 7 (elements "0123456789abcdef") `suchThatMap` commitSha

-- | A commit message: arbitrary words, with the occasional run of backticks
-- thrown in, which sits mid-line and so must not be read as a fence.
genCommitMessage :: Gen String
genCommitMessage = unwords <$> resize 3 (listOf1 word)
  where
    word = frequency [(4, getPrintableString <$> arbitrary), (1, pure "```")]

genCommit :: Gen Commit
genCommit = Commit <$> genChange <*> genCommitSha <*> genCommitMessage

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

parseTag :: String -> Either String (Change, String)
parseTag l
  | Just r <- tag "\128994 **Added**" = Right (Added, r)
  | Just r <- tag "\128308 **Removed**" = Right (Removed, r)
  | Just r <- tag "\128992 **Updated**" = Right (Updated (interdiff ""), r)
  | Just r <- tag "\9898 **Unchanged**" = Right (Unchanged, r)
  | otherwise = Left ("not a commit line: " ++ show l)
  where
    tag t = stripPrefix (t ++ " ") l

parseHeader :: String -> Either String Commit
parseHeader l = do
  (change, rest) <- parseTag l
  case break (== ' ') rest of
    (s, ' ' : commitMessage) -> case commitSha s of
      Just sha -> Right (Commit change sha commitMessage)
      Nothing -> Left ("commit line has no sha: " ++ show l)
    _ -> Left ("commit line has no commit message: " ++ show l)

toCommits :: [Block] -> Either String [Commit]
toCommits [] = Right []
toCommits (PlainLineOfText l : bs) = do
  commit <- parseHeader l
  case (cmChange commit, bs) of
    -- A fenced diff belongs to the updated commit right above it. The empty
    -- interdiff from 'parseTag' is the slot it fills.
    (Updated _, CodeBlock "diff" body : bs') ->
      (commit {cmChange = Updated (interdiff (unlines body))} :) <$> toCommits bs'
    _ -> (commit :) <$> toCommits bs
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
      forAll (resize 8 (listOf genCommit)) $ \commits ->
        cover 10 (any widened commits) "widened fence" $
          reparse (format commits) === Right commits
