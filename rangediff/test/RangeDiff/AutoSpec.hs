-- | Picking the way to take a range-diff: jj where the commits carry the change
-- ids it lines the two ranges up by, and git where they don't.
--
-- Which one was picked is not something 'RangeDiff.Auto.auto' says, so it is
-- read off the answer. Every case here rewrites a commit, and the two write an
-- interdiff nothing like one another, so the answer says outright which way
-- took it.
module RangeDiff.AutoSpec (spec) where

import RangeDiff qualified
import RangeDiff.Auto (auto)
import RangeDiff.CommitSha (CommitSha, shaText)
import RangeDiff.Git qualified
import RangeDiff.Jj qualified
import RangeDiff.Test.Git (git, revParse)
import RangeDiff.Test.Repo
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "auto" $ inRepos $ do
  it "takes the range-diff with jj where every commit carries a change id" $ \repo -> do
    (old, now) <- rewritten repo
    byJj <- takeRangeDiff RangeDiff.Jj.jj repo old now
    byGit <- takeRangeDiff RangeDiff.Git.git repo old now
    byJj `shouldNotBe` byGit
    takeRangeDiff auto repo old now `shouldReturn` byJj

  -- The same repo, as the tool meets it where it runs for real: a CI checkout
  -- is a plain git clone, and jj has to be able to say what it says there too.
  it "takes the range-diff with jj in a repo jj was never set up in" $ \repo -> do
    (old@(base, was), now@(_, is)) <- rewritten repo
    byJj <- takeRangeDiff RangeDiff.Jj.jj repo old now
    -- Holding the commits by sha alone: the way the tool leaves the repo it
    -- runs in, having fetched the commit the force-push left behind, which
    -- by then no ref of the clone leads to.
    withSystemTempDirectory "gh-post-range-diff" $ \dir -> do
      git dir ["init", "--quiet", "."]
      git dir (["fetch", "--quiet", repoDir repo] ++ map shaText [base, was, is])
      RangeDiff.rangeDiff (auto dir) base was base is `shouldReturn` byJj

  it "falls back on git where a commit carries none" $ \_ ->
    withPlainRepo pickedGit

-- | A branch of one commit, reworded and amended: the two versions of it.
rewritten :: Repo -> IO ((CommitSha, CommitSha), (CommitSha, CommitSha))
rewritten repo = do
  root <- initial repo
  [w] <- stack repo root [Patch "Add the widget" [("widget.py", "timeout = 30\n")]]
  old <- (,) <$> sha repo root <*> sha repo w
  reword repo w "Add a widget"
  amend repo w [("widget.py", "timeout = 5\n")]
  now <- (,) <$> sha repo root <*> sha repo w
  pure (old, now)

-- | A repo git built, so no commit of it carries one: what comes back has to
-- be what `git range-diff` comes out with. jj could be asked about this repo as
-- readily as about any other, and would answer -- under change ids it makes up
-- for commits that carry none, which name a rewritten commit and the commit it
-- was rewritten from as two unrelated commits. So the answer, not a failure, is
-- what says the fallback was taken.
pickedGit :: FilePath -> CommitSha -> CommitSha -> CommitSha -> Expectation
pickedGit dir base was now = do
  commits <- RangeDiff.rangeDiff (auto dir) base was base now
  wanted <- RangeDiff.rangeDiff (RangeDiff.Git.git dir) base was base now
  commits `shouldBe` wanted

-- | A repo built with git alone, holding a branch of one commit and a reword of
-- it: the commit the branch forks off, the commit it was on, and the one it is
-- on now.
withPlainRepo :: (FilePath -> CommitSha -> CommitSha -> CommitSha -> Expectation) -> Expectation
withPlainRepo act =
  withSystemTempDirectory "gh-post-range-diff" $ \dir -> do
    let run args = git dir (["-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
    run ["init", "--quiet", "."]
    -- How far git abbreviates a sha is its own business, and it varies with the
    -- size of the repo. Told not to abbreviate at all, `range-diff` prints the
    -- full shas.
    run ["config", "core.abbrev", "no"]
    commit dir run "root" "root\n" "root.txt"
    base <- revParse dir "HEAD"
    commit dir run "the one commit" "one\n" "c1.txt"
    was <- revParse dir "HEAD"
    run ["commit", "--quiet", "--amend", "--message=the one commit, reworded"]
    now <- revParse dir "HEAD"
    act dir base was now

-- | Commit one file with the given contents under the given message.
commit :: FilePath -> ([String] -> IO ()) -> String -> String -> FilePath -> IO ()
commit dir run message contents file = do
  writeFile (dir ++ "/" ++ file) contents
  run ["add", file]
  run ["commit", "--quiet", "--message=" ++ message]
