-- | A repo whose branch went through several versions.
module GhPostRangeDiff.Scenario
  ( Version (..),
    ChangeNo (..),
    Committed (..),
    Change (..),
    Scenario (..),
    versions,
    stateAt,
    regions,
    scenarioOf,
    shrinkScenario,
    valid,
    expectedInterdiff,
    Range (..),
    Repo,
    repoDir,
    git,
    rangeOf,
    baseMovement,
    shaOf,
    withRepo,
  )
where

import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import GhPostRangeDiff.Gen ()
import GhPostRangeDiff.Git (CommitSha, knownSha, shaText)
import GhPostRangeDiff.Git qualified as Git
import GhPostRangeDiff.RangeDiff qualified as RangeDiff
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Gen, arbitrary, choose, frequency, getPrintableString, shrink, shrinkList, suchThat, vectorOf)

-- * The scenario

-- | Which version of the branch, counted from zero: version 0 is what the
-- branch first was, and the last one is what it ended up as. It indexes the
-- states every change keeps, and the versions the repo was built into.
newtype Version = Version Int
  deriving (Eq, Show)

-- | Which change, counted from one over the whole scenario, base region
-- included. It is what a change commits to a file of its own under, so no two
-- commits in the repo ever touch the same file.
newtype ChangeNo = ChangeNo Int
  deriving (Eq, Show)

-- | What one version of the branch commits for a change: the message it commits
-- under, and the one line of its file we get to choose. Two versions that
-- commit the same thing hold the same commit; anything else is that commit
-- rewritten, whether the rewrite touched the message, the file, or both.
data Committed = Committed
  { cdMessage :: RangeDiff.CommitMessage,
    cdLine :: String
  }
  deriving (Eq, Show)

-- | One commit's life across every version of the branch: 'Nothing' where a
-- version doesn't carry it at all.
--
-- This is a "change" in the jujutsu sense: the stable identity of a commit, even
-- as it is rewritten.
--
-- The list holds one state per version, so it is always 'scVersions' long.
newtype Change = Change {chStates :: [Maybe Committed]}
  deriving (Show)

-- | What one version makes of a change.
stateAt :: Version -> Change -> Maybe Committed
stateAt (Version v) = (!! v) . chStates

-- | A whole repo's worth of scenario. Each version commits its changes one
-- after another from the root commit; the first 'scBaseChanges' of them go
-- under the fork point, and 'scChanges' go after the fork point.
data Scenario = Scenario
  { -- | how many versions of the branch there are; at least two
    scVersions :: Int,
    scBaseChanges :: [Change],
    scChanges :: [Change]
  }
  deriving (Show)

-- | Every version the scenario describes, oldest first.
versions :: Scenario -> [Version]
versions sc = map Version [0 .. scVersions sc - 1]

-- | Number every change from one, so no two commits in the repo ever touch the
-- same file, and hand back the base region and the range separately.
regions :: Scenario -> ([(ChangeNo, Change)], [(ChangeNo, Change)])
regions (Scenario _ baseChanges changes) =
  splitAt (length baseChanges) (zip (map ChangeNo [1 ..]) (baseChanges ++ changes))

-- | Is this a scenario we can actually build and diff? `git range-diff` refuses
-- an empty commit range, so every version has to keep at least one commit above
-- the fork point. The base region is free to be empty; a version that commits
-- nothing down there just forks from the root commit.
valid :: Scenario -> Bool
valid sc@(Scenario n _ changes) =
  n >= 2
    && all ((== n) . length . chStates) (scBaseChanges sc ++ scChanges sc)
    && and [any (isJust . stateAt v) changes | v <- versions sc]

-- | Scenarios with exactly @n@ versions of the branch.
scenarioOf :: Int -> Gen Scenario
scenarioOf n = (Scenario n <$> baseChanges <*> sizedChanges) `suchThat` valid
  where
    -- A third of the scenarios put nothing under the fork point, so every
    -- version forks from the root commit and the base has plainly not moved. A
    -- third bury a change or three down there, which usually moves the base by
    -- a force-push, since a change usually does something between one version
    -- and the next. The last third grow the base region instead, which moves it
    -- by an ordinary push.
    baseChanges =
      frequency
        [ (1, pure []),
          (1, choose (1, 3) >>= flip vectorOf (change n)),
          (1, growingBase n)
        ]

    -- Mostly short, because every change of every version really is committed,
    -- but often enough into double digits to exercise the leading space git
    -- pads a single-digit commit number out with once a range holds ten
    -- commits.
    sizedChanges = do
      k <- frequency [(3, choose (1, 6)), (1, choose (10, 12))]
      vectorOf k (change n)

-- | One change over @n@ versions: a commit it settles on, which most versions
-- keep as it is, some reword, some amend, some do both to, and some leave out
-- of the branch entirely.
change :: Int -> Gen Change
change n = do
  settled <- committed
  Change <$> vectorOf n (frequency (states settled))
  where
    states settled =
      [ (4, pure (Just settled)),
        (1, Just . (\l -> settled {cdLine = l}) <$> chosenLine),
        (1, Just . (\m -> settled {cdMessage = m}) <$> arbitrary),
        (1, Just <$> committed),
        (2, pure Nothing)
      ]

-- | A commit a version could carry.
committed :: Gen Committed
committed = Committed <$> arbitrary <*> chosenLine

-- | The one line of a change's file that a version gets to choose.
chosenLine :: Gen String
chosenLine = getPrintableString <$> arbitrary

-- | A base region that only ever grows: every version buries the changes the
-- one before it did under the fork point, committed exactly as they were, and
-- now and then one more. That leaves each version's base an ancestor of the
-- next one's, so the base moves by an ordinary push rather than by a
-- force-push. GitHub records no event for such a push, so the tool has to work
-- the old base out with @merge-base@ rather than read it off the timeline.
--
-- Growing is the only direction on offer. A base region that shrank again would
-- leave a later base under an earlier head, which is the ambiguity
-- "GhPostRangeDiff.RunSpec" throws a scenario out over.
growingBase :: Int -> Gen [Change]
growingBase n = do
  m <- choose (1, 3)
  -- The version each change is buried in, lowest change first, so that a change
  -- is never buried before the one beneath it. A change buried in version @n@
  -- never makes it into the base at all.
  starts <- sort <$> vectorOf m (choose (0, n))
  mapM buriedFrom starts
  where
    -- Absent until its version comes round, and committed exactly as it was
    -- from there on.
    buriedFrom v = do
      c <- committed
      pure (Change (replicate v Nothing ++ replicate (n - v) (Just c)))

-- | Shrink to another scenario we can build, smallest first: fewer versions,
-- then fewer changes, then simpler ones.
shrinkScenario :: Scenario -> [Scenario]
shrinkScenario sc@(Scenario n baseChanges changes) =
  filter valid $
    [dropVersion v sc | n > 2, v <- versions sc]
      ++ [sc {scBaseChanges = b} | b <- shrinkList shrinkChange baseChanges]
      ++ [sc {scChanges = s} | s <- shrinkList shrinkChange changes]

-- | Forget that one version of the branch ever existed. Every change loses the
-- same state, so they all stay 'scVersions' long.
dropVersion :: Version -> Scenario -> Scenario
dropVersion (Version v) (Scenario n baseChanges changes) =
  Scenario (n - 1) (map without baseChanges) (map without changes)
  where
    without ch = ch {chStates = take v (chStates ch) ++ drop (v + 1) (chStates ch)}

-- | Shrink one change, keeping one state per version.
shrinkChange :: Change -> [Change]
shrinkChange (Change states) =
  [Change settled | settled <- unified, settled /= states]
    ++ [Change states' | states' <- shrinkEach shrinkState states]
  where
    -- Every version equal to the initial version: a commit that just sits
    -- there untouched is the simplest thing a change can do, and it is the
    -- only shrink that puts a commit back into a version that dropped it.
    unified = [map (const (Just c)) states | c <- take 1 (catMaybes states)]

    shrinkState s = [Just c | Just c0 <- [s], c <- shrinkCommitted c0]

    -- The line only ever loses characters, never changes them, so it stays
    -- printable and stays one line, as 'change' generated it. The message has a
    -- shrinker of its own that keeps it committable.
    shrinkCommitted c =
      [c {cdLine = l} | l <- shrinkList (const []) (cdLine c)]
        ++ [c {cdMessage = m} | m <- shrink (cdMessage c)]

-- | Shrink one element at a time, leaving the list as long as it was.
shrinkEach :: (a -> [a]) -> [a] -> [[a]]
shrinkEach _ [] = []
shrinkEach f (x : xs) =
  [x' : xs | x' <- f x] ++ [x : xs' | xs' <- shrinkEach f xs]

-- * Building the repo

-- | The file a change commits to. One file each, so unrelated commits can never
-- be mistaken for one another.
file :: ChangeNo -> FilePath
file (ChangeNo n) = "f" ++ show n ++ ".txt"

-- | The @k@th body line of a change's file. Naming the change it belongs to
-- keeps two different changes' patches from resembling one another, which would
-- let range-diff pair a removed commit with an unrelated added one.
body :: ChangeNo -> Int -> String
body (ChangeNo n) k = "f" ++ show n ++ "-l" ++ show k

-- | The interdiff git prints for a rewritten commit, as 'RangeDiff.rangeDiff'
-- should hand it back: de-indented, so its own +/- sit in column 0. A rewrite
-- that reworded the commit gets a hunk over the metadata git shows a commit
-- under, one that amended it gets a hunk over the patch, and one that did both
-- gets both hunks, in that order.
expectedInterdiff :: ChangeNo -> Committed -> Committed -> RangeDiff.Interdiff
expectedInterdiff n a b = RangeDiff.interdiff (unlines (reworded ++ amended))
  where
    -- The message sits in a section of its own, indented by four, with the
    -- author above it and the head of the patch below — three lines of context
    -- either side, as a default @-U3@ diff leaves.
    reworded
      | cdMessage a == cdMessage b = []
      | otherwise =
          [ "@@ Metadata",
            " Author: " ++ auName author ++ " <" ++ auEmail author ++ ">",
            " ",
            "  ## Commit message ##",
            "-    " ++ RangeDiff.messageText (cdMessage a),
            "+    " ++ RangeDiff.messageText (cdMessage b),
            " ",
            "  ## " ++ file n ++ " (new) ##",
            " @@"
          ]

    -- Context here is the tail of the file's body, as the new patch adds it.
    amended
      | cdLine a == cdLine b = []
      | otherwise =
          ["@@ " ++ file n ++ " (new)"]
            ++ [" +" ++ body n k | k <- [6 .. 8]]
            ++ ["-+" ++ cdLine a, "++" ++ cdLine b]

-- | A change's file, ending on the line the version carries. The body is long
-- enough that rewriting that one line is a small fraction of the commit, which
-- keeps range-diff pairing the two versions of it (marker @!@) instead of
-- treating them as an unrelated removal plus addition.
contents :: ChangeNo -> String -> String
contents n line = unlines ([body n k | k <- [1 .. 8]] ++ [line])

-- | Who commits everything in the repo. The model needs it too: it is the one
-- line of commit metadata a range-diff shows around a reworded message.
data Author = Author
  { auName :: String,
    auEmail :: String
  }

author :: Author
author = Author {auName = "t", auEmail = "t@t"}

-- | Run a git command in whichever repo is current, and hand back its output.
git :: [String] -> IO String
git = Git.sh "git"

-- | Commit a single file with the given contents and message.
commit :: FilePath -> String -> String -> IO ()
commit name text msg = do
  writeFile name text
  _ <- git ["add", name]
  _ <- git ["commit", "-q", "-m", msg]
  pure ()

-- | Where one version of the branch begins and ends: the commit its range is
-- taken from, and its tip. This is the range `git range-diff` is handed.
data Range = Range
  { rgBase :: CommitSha,
    rgHead :: CommitSha
  }

-- | What one version of the branch was built into.
data Built = Built
  { btRange :: Range,
    -- | the commit each change it carries became
    btShas :: [(ChangeNo, CommitSha)]
  }

-- | The repo a scenario was built into: where it is, and one build per version,
-- in the same order as the scenario's states.
data Repo = Repo FilePath [Built]

-- | Where the repo is, so that whatever else is done with it (opening a pull
-- request on it, cloning it) can be done from anywhere.
repoDir :: Repo -> FilePath
repoDir (Repo dir _) = dir

-- | The range one version of the branch spans.
rangeOf :: Repo -> Version -> Range
rangeOf repo v = btRange (builtAt repo v)

-- | What the base under the branch did between two versions, read off the repo
-- rather than guessed at, so a scenario can be told what its base region did.
-- Only good for saying what a scenario covered.
--
-- A base that advanced got there by an ordinary push, which leaves no
-- force-push event to reconstruct it from; a base that was forced leaves one.
baseMovement :: Repo -> Version -> Version -> IO String
baseMovement repo v w
  | base v == base w = pure "shared"
  | otherwise = do
      fastForward <- Git.isAncestor (base v) (base w)
      pure (if fastForward then "advanced" else "forced")
  where
    base = rgBase . rangeOf repo

-- | The commit one version made of one change. Only ask about a change that
-- version carries, since there is no commit to name otherwise.
shaOf :: Repo -> Version -> ChangeNo -> CommitSha
shaOf repo v@(Version i) n@(ChangeNo k) =
  fromMaybe
    (error ("change " ++ show k ++ " is not in version " ++ show i))
    (lookup n (btShas (builtAt repo v)))

builtAt :: Repo -> Version -> Built
builtAt (Repo _ builts) (Version v) = builts !! v

-- | Build the scenario into a throwaway repo and run the action in it.
withRepo :: Scenario -> (Repo -> IO a) -> IO a
withRepo sc act = withSystemTempDirectory "gh-post-range-diff" $ \dir -> withCurrentDirectory dir $ do
  _ <- git ["init", "-q"]
  _ <- git ["config", "user.email", auEmail author]
  _ <- git ["config", "user.name", auName author]
  -- How far git abbreviates a sha is its own business, and it varies with the
  -- size of the repo. Told not to abbreviate at all, `range-diff` prints the
  -- full shas, which are the ones the scenario knows its commits by.
  _ <- git ["config", "core.abbrev", "no"]

  -- Every version has to start somewhere, and they all start here, on one and
  -- the same commit.
  commit "root.txt" "root\n" "root"
  root <- Git.revParse "HEAD"

  let (baseChanges, changes) = regions sc
      -- Commit a version's changes from the root up, stopping at the end of the
      -- base region to note the commit its range should be taken from.
      buildVersion v@(Version i) = do
        let branch = "v" ++ show i
        _ <- git ["checkout", "-q", "-b", branch, shaText root]
        mapM_ (commitChange v) baseChanges
        base <- Git.revParse "HEAD"
        mapM_ (commitChange v) changes
        tip <- Git.revParse "HEAD"
        Built (Range base tip) <$> changeShas base branch v changes
  builts <- mapM buildVersion (versions sc)
  act (Repo dir builts)
  where
    commitChange v (n, ch) = case stateAt v ch of
      Nothing -> pure ()
      Just c -> commit (file n) (contents n (cdLine c)) (RangeDiff.messageText (cdMessage c))

-- | Which change ended up as which commit on one version of the branch.
-- `rev-list` is newest first, so it lines up with the scenario once reversed.
changeShas :: CommitSha -> String -> Version -> [(ChangeNo, Change)] -> IO [(ChangeNo, CommitSha)]
changeShas base branch v changes = do
  shas <- reverse . lines <$> git ["rev-list", shaText base ++ ".." ++ branch]
  pure (zip [n | (n, ch) <- changes, isJust (stateAt v ch)] (map knownSha shas))
