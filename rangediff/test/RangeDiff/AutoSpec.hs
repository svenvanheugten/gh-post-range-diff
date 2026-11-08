-- | Picking the way to take a range-diff: jj where the commits carry the change
-- ids it lines the two ranges up by, and git where they don't.
--
-- Which one was picked is not something 'RangeDiff.Auto.auto' says, so it is
-- read off the answer. Every case here rewrites a commit, and the two write an
-- interdiff nothing like one another, so the answer says outright which way
-- took it.
--
-- The tool runs in a CI checkout, so that is where every case is taken but
-- one: see 'inCheckout'. The one is a repo somebody keeps with jj themselves.
module RangeDiff.AutoSpec (spec) where

import Control.Monad (void)
import Data.List (nub)
import Data.List.Extra (trim)
import RangeDiff qualified
import RangeDiff.Auto (auto)
import RangeDiff.CommitSha (CommitSha, knownSha, shaText)
import RangeDiff.Git qualified
import RangeDiff.Jj qualified
import RangeDiff.Test.Repo
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "auto" $ inRepos $ do
  it "takes the range-diff with jj where every commit carries a change id" $ \repo -> do
    (old, now) <- rewritten repo
    inCheckout pickedJj (repoDir repo) old now

  it "falls back on git where a commit carries none" $ \_ ->
    withPlainRepo (inCheckout pickedGit)

  it "falls back on git where only some of the commits carry one" $ \_ ->
    withMixedRepo (inCheckout pickedGit)

  -- Where the branch was built: jj holds every commit already, and is asked
  -- about the repo as it is.
  it "takes the range-diff with jj in a repo somebody keeps with jj" $ \repo -> do
    (old, now) <- rewritten repo
    pickedJj (repoDir repo) old now

-- | Run @expect@ on the two versions of a branch the way the tool meets them
-- where it runs for real: in a CI checkout of the repo in @origin@. That is a
-- plain git clone, which jj was never set up in, and it holds the commits by
-- sha alone, having fetched them: by then no ref of it leads to the commit the
-- force-push left behind.
inCheckout :: (FilePath -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> Expectation) -> FilePath -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> Expectation
inCheckout expect origin old@(oldBase, oldHead) now@(newBase, newHead) =
  withSystemTempDirectory "checkout" $ \dir -> do
    gitIn dir ["init", "--quiet", "."]
    gitIn dir (["fetch", "--quiet", origin] ++ map shaText (nub [oldBase, oldHead, newBase, newHead]))
    expect dir old now

-- | Expect 'auto' to take the range-diff of @old@ against @now@ in the repo in
-- @dir@ the way @way@ takes it, and not the way @other@ does.
--
-- Which way was taken is read off the answer, so the two have to answer
-- differently for it to say anything. 'auto' goes first, while the repo is as
-- the example left it: jj sets up a repo it is asked about, and 'auto' has to
-- be able to do without that.
picked :: (FilePath -> RangeDiff.Handle) -> (FilePath -> RangeDiff.Handle) -> FilePath -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> Expectation
picked way other dir (oldBase, oldHead) (newBase, newHead) = do
  commits <- takeWith auto
  wanted <- takeWith way
  unwanted <- takeWith other
  wanted `shouldNotBe` unwanted
  commits `shouldBe` wanted
  where
    takeWith handle = RangeDiff.rangeDiff (handle dir) oldBase oldHead newBase newHead

-- | Expect 'auto' to take the range-diff with jj.
pickedJj :: FilePath -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> Expectation
pickedJj = picked RangeDiff.Jj.jj RangeDiff.Git.git

-- | Expect 'auto' to take the range-diff with git. jj could be asked about a
-- commit that carries no change id as readily as about any other, and would
-- answer -- under a change id it makes up for it, which names a rewritten
-- commit and the commit it was rewritten from as two unrelated commits. So the
-- answer, not a failure, is what says the fallback was taken.
pickedGit :: FilePath -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> Expectation
pickedGit = picked RangeDiff.Git.git RangeDiff.Jj.jj

-- | A branch of one commit, reworded and amended: the two versions of it.
rewritten :: Repo -> IO ((CommitSha, CommitSha), (CommitSha, CommitSha))
rewritten repo = do
  root <- initial repo
  base <- sha repo root
  (was, is) <- rewrittenOn repo root
  pure ((base, was), (base, is))

-- | A commit made on @parent@, then reworded and amended: the commit it was,
-- and the one it is now. The amend leaves most of the commit's patch alone, so
-- that git still pairs the two versions up.
rewrittenOn :: Repo -> ChangeId -> IO (CommitSha, CommitSha)
rewrittenOn repo parent = do
  [w] <- stack repo parent [Patch "Add the widget" [("widget.py", widget "30")]]
  was <- sha repo w
  reword repo w "Add a widget"
  amend repo w [("widget.py", widget "5")]
  is <- sha repo w
  pure (was, is)
  where
    widget timeout =
      unlines
        [ "class Widget:",
          "    def __init__(self, name):",
          "        self.name = name",
          "        self.timeout = " ++ timeout,
          "",
          "    def __repr__(self):",
          "        return f\"Widget({self.name!r})\""
        ]

-- | A repo built with git alone, holding a branch of one commit and a reword of
-- it: the repo, and the two versions of the branch.
withPlainRepo :: (FilePath -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> Expectation) -> Expectation
withPlainRepo act =
  withGitRepo $ \dir run base -> do
    commit dir run "the one commit" "one\n" "c1.txt"
    was <- revParse dir "HEAD"
    run ["commit", "--quiet", "--amend", "--message=the one commit, reworded"]
    now <- revParse dir "HEAD"
    act dir (base, was) (base, now)

-- | A repo git started and jj carried on, holding a branch of two commits and
-- a rewrite of it. git made the first commit, and jj, set up over the repo
-- after that, the second, so only the second carries a change id. That one is
-- the one rewritten. The repo, and the two versions of the branch.
withMixedRepo :: (FilePath -> (CommitSha, CommitSha) -> (CommitSha, CommitSha) -> Expectation) -> Expectation
withMixedRepo act =
  withGitRepo $ \dir run base -> do
    commit dir run "Add a gadget" "class Gadget:\n    pass\n" "gadget.py"
    (repo, gadget) <- colocate dir
    (was, is) <- rewrittenOn repo gadget
    act dir (base, was) (base, is)

-- | A repo built with git, holding nothing yet but the commit a branch is to
-- fork off: the repo, a way to run git in it under a name to commit with, and
-- that commit.
withGitRepo :: (FilePath -> ([String] -> IO ()) -> CommitSha -> Expectation) -> Expectation
withGitRepo act =
  withSystemTempDirectory "gh-post-range-diff" $ \dir -> do
    let run args = gitIn dir (["-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
    run ["init", "--quiet", "."]
    commit dir run "root" "root\n" "root.txt"
    act dir run =<< revParse dir "HEAD"

-- | Commit one file with the given contents under the given message.
commit :: FilePath -> ([String] -> IO ()) -> String -> String -> FilePath -> IO ()
commit dir run message contents file = do
  writeFile (dir ++ "/" ++ file) contents
  run ["add", file]
  run ["commit", "--quiet", "--message=" ++ message]

-- | Run a git command in the repo in @dir@. Nothing here reads what one prints,
-- but it is captured all the same, to keep it out of the test output.
gitIn :: FilePath -> [String] -> IO ()
gitIn dir = void . RangeDiff.Git.command dir

-- | The commit a revision names, so that a repo built with git can be read
-- back.
revParse :: FilePath -> String -> IO CommitSha
revParse dir rev = knownSha . trim <$> RangeDiff.Git.command dir ["rev-parse", rev]
