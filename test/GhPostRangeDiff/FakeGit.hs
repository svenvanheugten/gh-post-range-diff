-- | A git repo with nothing underneath it: no directory, no processes, no
-- files. All it holds is commits — each one naming its parent — and the
-- branches pointing at them. That is enough to answer everything a
-- 'Git.Handle' is asked, since none of it is about what a commit holds, only
-- about how the commits relate.
--
-- The one thing a repo can't answer for is a commit it hasn't got: those are
-- errors here, as they are for git. That is what keeps the tool honest about
-- fetching what it is going to look at.
module GhPostRangeDiff.FakeGit
  ( ChangeNo (..),
    Change (..),
    Commit (..),
    commit,
    Repo,
    new,
    clone,
    store,
    place,
    handle,
  )
where

import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GhPostRangeDiff.Git qualified as Git
import RangeDiff.CommitSha (CommitSha, knownSha, shaText)

-- * The commits

-- | What a commit is, before it is one: the change it carries, and the change
-- it sits on. A change number is the only thing that tells two commits apart
-- here — nothing else about a commit is ever asked of a 'Git.Handle' — so a
-- chain of them is the whole of a history.
newtype ChangeNo = ChangeNo Int deriving (Eq, Ord, Show)

data Change = Change
  { changeNo :: ChangeNo,
    changeParent :: Maybe Change
  }
  deriving (Eq, Show)

-- | A commit as these repos have it: the sha it is known by, and what it sits
-- on.
data Commit = Commit
  { cmSha :: CommitSha,
    cmParent :: Maybe CommitSha
  }
  deriving (Eq, Show)

-- | The commit a change comes out as. Its sha stands for everything under it
-- as well as the change itself, the way a git sha does: rewriting a commit
-- rewrites every commit above it, so a change carried on a different history
-- is a different commit.
commit :: Change -> Commit
commit ch = Commit (shaOf ch) (cmSha . commit <$> changeParent ch)

-- Make up a sha for a change and everything under it, by taking the SHA-1 of
-- the chain of change numbers. Not the sha git would give the commit, which
-- would mean hashing content these commits haven't got, but a real sha all the
-- same: forty hex digits, and different for chains that differ anywhere.
shaOf :: Change -> CommitSha
shaOf ch = knownSha (Char8.unpack (Base16.encode (SHA1.hash (Char8.pack (show (numbers ch))))))
  where
    numbers c = changeNo c : maybe [] numbers (changeParent c)

-- * The repo

-- | What a repo is: the commits it holds, the branches it has, and the repo it
-- fetches from, where it has one.
data Repo = Repo
  { rpOrigin :: Maybe Repo,
    rpCommits :: IORef (Map CommitSha Commit),
    rpBranches :: IORef (Map String CommitSha)
  }

-- | A repo of its own, with nothing in it and nowhere to fetch from.
new :: IO Repo
new = Repo Nothing <$> newIORef Map.empty <*> newIORef Map.empty

-- | A repo that fetches from another one. It starts out empty, which is the
-- point of it: whatever the tool means to look at, it has to go and get.
clone :: Repo -> IO Repo
clone origin = Repo (Just origin) <$> newIORef Map.empty <*> newIORef Map.empty

-- | Put commits in the repo, the way pushing a branch puts every commit under
-- it there.
store :: Repo -> [Commit] -> IO ()
store repo cs =
  modifyIORef' (rpCommits repo) (Map.union (Map.fromList [(cmSha c, c) | c <- cs]))

-- | Put a branch on a commit.
place :: Repo -> String -> CommitSha -> IO ()
place repo b s = modifyIORef' (rpBranches repo) (Map.insert b s)

-- | Everything the tool does with a repo, answered by walking the commits.
handle :: Repo -> Git.Handle
handle repo =
  Git.Handle
    { Git.fetch = fetch repo,
      Git.isAncestorOf = \a b -> asking (\m -> isAncestorOf m a b),
      Git.mergeBase = \a b -> asking (\m -> mergeBase m a b)
    }
  where
    asking f = f <$> readIORef (rpCommits repo)

-- | Bring a branch in from origin, and the commits named alongside it, and
-- hand back where that branch is. What is under them comes too: a fetch brings
-- the history of whatever it fetches.
fetch :: Repo -> String -> [CommitSha] -> IO CommitSha
fetch repo b ss = do
  origin <- maybe (fail "fetch: the repo has no origin") pure (rpOrigin repo)
  m <- readIORef (rpCommits origin)
  branches <- readIORef (rpBranches origin)
  let tip = fromMaybe (error ("origin has no branch " ++ b)) (Map.lookup b branches)
  store repo (concatMap (ancestry m) (tip : ss))
  pure tip

-- * Reading the commits

-- | The commit a sha names. A repo can only answer for what it holds, so a sha
-- it hasn't got is an error rather than a 'Nothing' to be swallowed.
lookedUp :: Map CommitSha Commit -> CommitSha -> Commit
lookedUp m s = fromMaybe (error ("no such commit: " ++ shaText s)) (Map.lookup s m)

-- | A commit and every commit under it, nearest first.
ancestry :: Map CommitSha Commit -> CommitSha -> [Commit]
ancestry m s = c : maybe [] (ancestry m) (cmParent c)
  where
    c = lookedUp m s

-- | Whether @a@ is @b@ or a commit under it. Both have to be commits the repo
-- holds: @a@ is looked up rather than just compared against, so asking about
-- one that was never fetched is an error and not a no.
isAncestorOf :: Map CommitSha Commit -> CommitSha -> CommitSha -> Bool
isAncestorOf m a b = cmSha (lookedUp m a) `elem` shas (ancestry m b)

-- | The nearest commit both of them are on. History here is a tree — every
-- commit has one parent at most — so the first commit under @b@ that is also
-- under @a@ is the nearest such.
mergeBase :: Map CommitSha Commit -> CommitSha -> CommitSha -> CommitSha
mergeBase m a b = case filter (`elem` shas (ancestry m a)) (shas (ancestry m b)) of
  (s : _) -> s
  -- git exits non-zero here, which the tool doesn't survive either.
  [] -> error "no merge base"

shas :: [Commit] -> [CommitSha]
shas = map cmSha
