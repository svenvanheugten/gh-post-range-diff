-- | A repo whose branch went through several versions.
module GhPostRangeDiff.Scenario
  ( Version (..),
    ChangeNo (..),
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
    rangeOf,
    shaOf,
    withRepo,
  )
where

import Data.Maybe (catMaybes, fromMaybe, isJust)
import GhPostRangeDiff.Gen ()
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

-- | One commit's life across every version of the branch: 'Nothing' where a
-- version doesn't carry it at all, and @Just l@ where it does, with @l@ the one
-- line of its file we get to choose. Two versions holding the same line are the
-- same commit; different lines are that commit rewritten.
--
-- This is a "change" in the jujutsu sense: the stable identity of a commit, even
-- as it is rewritten.
--
-- The list holds one state per version, so it is always 'scVersions' long.
data Change = Change
  { chStates :: [Maybe String],
    chMessage :: RangeDiff.CommitMessage
  }
  deriving (Show)

-- | What one version makes of a change.
stateAt :: Version -> Change -> Maybe String
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
    -- Half the scenarios put nothing under the fork point, so every version
    -- forks from the root commit and the base has plainly not moved. The other
    -- half bury a change or three down there, which usually moves it, since a
    -- change usually does something between one version and the next.
    baseChanges = frequency [(1, pure []), (1, choose (1, 3) >>= flip vectorOf (change n))]

    -- Mostly short, because every change of every version really is committed,
    -- but often enough into double digits to exercise the leading space git
    -- pads a single-digit commit number out with once a range holds ten
    -- commits.
    sizedChanges = do
      k <- frequency [(3, choose (1, 6)), (1, choose (10, 12))]
      vectorOf k (change n)

-- | One change over @n@ versions: a line it settles on, which most versions
-- keep, some rewrite, and some leave the commit out over entirely.
change :: Int -> Gen Change
change n = Change <$> states <*> arbitrary
  where
    states = do
      settled <- line
      vectorOf n (frequency [(3, pure (Just settled)), (1, Just <$> line), (1, pure Nothing)])
    line = getPrintableString <$> arbitrary

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
shrinkChange ch@(Change states message) =
  [ch {chStates = settled} | settled <- unified, settled /= states]
    ++ [ch {chStates = states'} | states' <- shrinkEach shrinkState states]
    ++ [ch {chMessage = message'} | message' <- shrink message]
  where
    -- Every version equal to the initial version: a commit that just sits
    -- there untouched is the simplest thing a change can do, and it is the
    -- only shrink that puts a commit back into a version that dropped it.
    unified = [map (const (Just l)) states | l <- take 1 (catMaybes states)]

    -- Only ever drop characters, never change them: the line stays printable
    -- and stays one line, as 'change' generated it.
    shrinkState s = [Just l | Just l0 <- [s], l <- shrinkList (const []) l0]

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
-- should hand it back: de-indented, so its own +/- sit in column 0. It holds
-- the rewritten hunk header, the three lines of context a default @-U3@ diff
-- leaves before the change (the tail of the file's body), then the last line as
-- the old patch had it and as the new one has it.
expectedInterdiff :: ChangeNo -> String -> String -> RangeDiff.Interdiff
expectedInterdiff n a b =
  RangeDiff.interdiff . unlines $
    ["@@ " ++ file n ++ " (new)"]
      ++ [" +" ++ body n k | k <- [6 .. 8]]
      ++ ["-+" ++ a, "++" ++ b]

-- | A change's file, ending on the line the version carries. The body is long
-- enough that rewriting that one line is a small fraction of the commit, which
-- keeps range-diff pairing the two versions of it (marker @!@) instead of
-- treating them as an unrelated removal plus addition.
contents :: ChangeNo -> String -> String
contents n line = unlines ([body n k | k <- [1 .. 8]] ++ [line])

git :: [String] -> IO String
git = Git.sh "git"

-- | Commit a single file with the given contents and message.
commit :: FilePath -> String -> String -> IO ()
commit name text msg = do
  writeFile name text
  _ <- git ["add", name]
  _ <- git ["commit", "-q", "-m", msg]
  pure ()

-- | Read a sha git just printed, which we know to be one.
knownSha :: String -> RangeDiff.CommitSha
knownSha s = fromMaybe (error ("not a sha: " ++ s)) (RangeDiff.commitSha s)

-- | Where one version of the branch begins and ends: the commit its range is
-- taken from, and its tip. This is the range `git range-diff` is handed.
data Range = Range
  { rgBase :: RangeDiff.CommitSha,
    rgHead :: RangeDiff.CommitSha
  }

-- | What one version of the branch was built into.
data Built = Built
  { btRange :: Range,
    -- | the commit each change it carries became
    btShas :: [(ChangeNo, RangeDiff.CommitSha)]
  }

-- | The repo a scenario was built into: one build per version, in the same
-- order as the scenario's states.
newtype Repo = Repo [Built]

-- | The range one version of the branch spans.
rangeOf :: Repo -> Version -> Range
rangeOf repo v = btRange (builtAt repo v)

-- | The commit one version made of one change. Only ask about a change that
-- version carries, since there is no commit to name otherwise.
shaOf :: Repo -> Version -> ChangeNo -> RangeDiff.CommitSha
shaOf repo v@(Version i) n@(ChangeNo k) =
  fromMaybe
    (error ("change " ++ show k ++ " is not in version " ++ show i))
    (lookup n (btShas (builtAt repo v)))

builtAt :: Repo -> Version -> Built
builtAt (Repo builts) (Version v) = builts !! v

-- | Build the scenario into a throwaway repo and run the action in it.
withRepo :: Scenario -> (Repo -> IO a) -> IO a
withRepo sc act = withSystemTempDirectory "gh-post-range-diff" $ \dir -> withCurrentDirectory dir $ do
  _ <- git ["init", "-q"]
  _ <- git ["config", "user.email", "t@t"]
  _ <- git ["config", "user.name", "t"]
  -- How far git abbreviates a sha is its own business, and it varies with the
  -- size of the repo. Told not to abbreviate at all, `range-diff` prints the
  -- full shas, which are the ones the scenario knows its commits by.
  _ <- git ["config", "core.abbrev", "no"]

  -- Every version has to start somewhere, and they all start here, on one and
  -- the same commit.
  commit "root.txt" "root\n" "root"
  root <- knownSha <$> Git.revParse "HEAD"

  let (baseChanges, changes) = regions sc
      -- Commit a version's changes from the root up, stopping at the end of the
      -- base region to note the commit its range should be taken from.
      buildVersion v@(Version i) = do
        let branch = "v" ++ show i
        _ <- git ["checkout", "-q", "-b", branch, RangeDiff.shaText root]
        mapM_ (commitChange v) baseChanges
        base <- knownSha <$> Git.revParse "HEAD"
        mapM_ (commitChange v) changes
        tip <- knownSha <$> Git.revParse "HEAD"
        Built (Range base tip) <$> changeShas base branch v changes
  builts <- mapM buildVersion (versions sc)
  act (Repo builts)
  where
    commitChange v (n, ch) = case stateAt v ch of
      Nothing -> pure ()
      Just line -> commit (file n) (contents n line) (RangeDiff.messageText (chMessage ch))

-- | Which change ended up as which commit on one version of the branch.
-- `rev-list` is newest first, so it lines up with the scenario once reversed.
changeShas :: RangeDiff.CommitSha -> String -> Version -> [(ChangeNo, Change)] -> IO [(ChangeNo, RangeDiff.CommitSha)]
changeShas base branch v changes = do
  shas <- reverse . lines <$> git ["rev-list", RangeDiff.shaText base ++ ".." ++ branch]
  pure (zip [n | (n, ch) <- changes, isJust (stateAt v ch)] (map knownSha shas))
