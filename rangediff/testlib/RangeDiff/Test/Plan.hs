-- | The whole of what is going to happen to a repo, worked out as one pure
-- value before the repo even exists.
--
-- A plan says what the branch starts out as (the commits under the fork point
-- and the commits above it) and what every rewrite of it does to the version
-- before it. Nothing in here has a repo to look at: a commit is no more than
-- the message it goes under and the one line of its file it chooses, and a
-- rewrite no more than what it does to the commits that are there.
-- "RangeDiff.Test.Repo" is what carries a plan out.
module RangeDiff.Test.Plan
  ( Plan (..),
    Step (..),
    Action (..),
    Committed (..),
    Shape (..),
    outcome,
    everyRewrite,
    plan,
    shrinkPlan,
  )
where

import Control.Monad (replicateM)
import Data.Char (isPrint)
import Data.List (intercalate)
import RangeDiff qualified
import RangeDiff.Test.Gen ()
import Test.QuickCheck (Gen, arbitrary, choose, frequency, getPrintableString, shrink, shrinkList, suchThat)

-- * What a scenario is going to do

-- | What a version commits for one commit of the branch: the message it goes
-- under, and the one line of its file the version gets to choose.
data Committed = Committed
  { cdMessage :: RangeDiff.CommitMessage,
    cdLine :: String
  }
  deriving (Eq)

-- | What one version does to one commit of the version before it, or puts into
-- it.
data Action
  = -- | leave the commit it falls to exactly as it is
    Keep
  | -- | rewrite the commit it falls to into this
    Rework Committed
  | -- | drop the commit it falls to from the branch
    Drop
  | -- | put a commit in here that wasn't there
    Insert Committed

-- | What one version does to the version before it: something to the stretch
-- under the fork point, and something to the stretch above it.
--
-- Each is a list of actions read against the commits that are there, bottom
-- commit first. An 'Insert' falls between two of them and doesn't align
-- with an existing commit.
data Step = Step
  { stpBase, stpBranch :: [Action]
  }

-- | A whole scenario, worked out before a repo to carry it out in even exists:
-- the version of the branch a repo starts out with (the commits under the fork
-- point and the commits above it, bottom commit first) and every rewrite of it
-- that follows.
data Plan = Plan
  { plBase, plBranch :: [Committed],
    plSteps :: [Step]
  }

-- | The branch as a plan has it partway through: the commits under the fork
-- point and the commits above it.
data Shape = Shape
  { shBase, shBranch :: [Committed]
  }
  deriving (Eq)

-- * Planning one

-- | A scenario: a first version of the branch, and between @lo@ and @hi@
-- rewrites of it.
plan :: (Int, Int) -> Gen Plan
plan bounds = do
  base <- baseCommits
  branch <- branchCommits
  n <- choose bounds
  Plan base branch <$> steps n (Shape base branch)

-- | @n@ rewrites in a row, each worked out from the branch the ones before it
-- leave behind.
steps :: Int -> Shape -> Gen [Step]
steps n sh
  | n <= 0 = pure []
  | otherwise = do
      s <- mutations sh
      (s :) <$> steps (n - 1) (outcome sh s)

-- | What the next version does to the branch: something to the stretch under
-- the fork point, and something to the stretch above it, and between them
-- something a scenario can hold.
mutations :: Shape -> Gen Step
mutations sh = (Step <$> stretchPlan (shBase sh) <*> stretchPlan (shBranch sh)) `suchThat` pushable sh

-- | What the next version does to a stretch of branch: something to every
-- commit that is there, and now and then a commit or two that weren't,
-- wherever they fit.
--
-- Half the time it leaves every commit that is there exactly as it is and only
-- puts commits on top of them. That's especially interesting when for the base
-- branch, since fast-forwards on the base branch aren't recorded on the GitHub
-- timeline, and need to be handled differently by the tool. As such, we want it
-- to happen relatively often.
stretchPlan :: [Committed] -> Gen [Action]
stretchPlan cs = frequency [(1, onTop), (1, throughout)]
  where
    onTop = (map (const Keep) cs ++) <$> insertions
    throughout = do
      bottom <- inserted
      rest <- mapM step cs
      pure (bottom ++ concat rest)
    step c = (:) <$> fate c <*> inserted
    fate c =
      frequency
        [ (4, pure Keep),
          (3, Rework <$> reworked c),
          (2, pure Drop)
        ]
    -- What goes into one gap: usually nothing, and where something does, now
    -- and then more than one commit, so that a version can come out with a run
    -- of commits nobody has seen before standing next to each other.
    --
    -- A gap takes about as many commits as it ever did: it fills a little less
    -- often than it used to, and where it does it now and then brings more than
    -- one. Every commit put in is another commit to build, so runs any commoner
    -- than this would cost a scenario real time.
    inserted = frequency [(9, pure []), (1, insertions)]

-- | A run of commits put into one gap: one, and now and then more.
insertions :: Gen [Action]
insertions = (:) . Insert <$> committed <*> frequency [(2, pure []), (1, insertions)]

-- | A commit rewritten: reworded, amended, or both. It really is rewritten,
-- since a rewrite that left the commit as it was is not one.
reworked :: Committed -> Gen Committed
reworked c = rewritten `suchThat` (/= c)
  where
    rewritten =
      frequency
        [ (1, (\m -> c {cdMessage = m}) <$> arbitrary),
          (1, (\l -> c {cdLine = l}) <$> line),
          (1, committed)
        ]

-- | How many commits the branch starts out with above the fork point. Mostly a
-- handful, because every one of them really is committed, but often enough into
-- double digits to exercise the leading space git pads a single-digit commit
-- number out with once a range holds ten commits.
branchCommits :: Gen [Committed]
branchCommits = flip replicateM committed =<< frequency [(3, choose (1, 6)), (1, choose (10, 12))]

-- | How many commits it starts out with under the fork point. Often none at
-- all, so that it forks straight off the root commit.
baseCommits :: Gen [Committed]
baseCommits = flip replicateM committed =<< frequency [(1, pure 0), (1, choose (1, 3))]

-- | A commit a version could carry.
committed :: Gen Committed
committed = Committed <$> arbitrary <*> line

-- | The one line of a commit's file that the version carrying it chooses.
line :: Gen String
line = getPrintableString <$> arbitrary

-- * Reading a plan without a repo

-- | The branch a rewrite leaves behind.
outcome :: Shape -> Step -> Shape
outcome sh s = Shape (applied (shBase sh) (stpBase s)) (applied (shBranch sh) (stpBranch s))

-- | A stretch of branch with a plan's actions read against it, bottom commit
-- first.
--
-- This is "RangeDiff.Test.Repo"'s @apply@ with the repo taken out of it,
-- and the two have to agree: what a plan says a stretch becomes is what the
-- repo it is carried out in has to end up holding.
applied :: [Committed] -> [Action] -> [Committed]
applied cs [] = cs
applied cs (a : as) = case a of
  Insert d -> d : applied cs as
  Keep -> next (\c rest -> c : applied rest as)
  Rework d -> next (\_ rest -> d : applied rest as)
  Drop -> next (\_ rest -> applied rest as)
  where
    next f = case cs of
      [] -> error "plan acts on a commit that isn't there"
      c : rest -> f c rest

-- | Whether a rewrite is one that could be pushed.
--
-- It has to leave a range for `git range-diff` to be taken over, so at least
-- one commit stays above the fork point. And it has to do something: a version
-- that came out as the one before it is a version nobody ever pushed.
--
-- Whether it did anything is read off the shape alone, which is the careful way
-- round. Two commits carrying the same thing are still different commits in the
-- repo, so a rewrite this calls a no-op may well have been a push — but one it
-- lets through always is one.
pushable :: Shape -> Step -> Bool
pushable sh s = not (null (shBranch sh')) && sh' /= sh
  where
    sh' = outcome sh s

-- | Whether a plan is one 'plan' could have come up with: a branch to take a
-- range-diff over throughout, at least one rewrite, and every rewrite one that
-- could be pushed.
plannable :: Plan -> Bool
plannable p =
  not (null (plBranch p))
    && not (null (plSteps p))
    && everyRewrite pushable p

-- | Whether something holds of every rewrite in a plan, each read against the
-- branch the rewrites before it left behind.
--
-- Every rewrite a plan holds is one that could be pushed, and that is the whole
-- of what a plan promises. A property that can only be run on some of them says
-- which with this.
everyRewrite :: (Shape -> Step -> Bool) -> Plan -> Bool
everyRewrite ok p = go (Shape (plBase p) (plBranch p)) (plSteps p)
  where
    go _ [] = True
    go sh (s : ss) = ok sh s && go (outcome sh s) ss

-- * Shrinking one

-- | Smaller scenarios to try once one has failed: fewer rewrites, fewer commits
-- to start out with, less done by any one rewrite, and smaller messages and
-- lines throughout.
--
-- Nothing here has to keep a plan coherent on its own: a candidate is
-- 'normalize'd back into one that says what becomes of the commits that are
-- there and nothing else, and thrown away unless it is 'plannable'.
shrinkPlan :: Plan -> [Plan]
shrinkPlan p =
  filter plannable . map normalize $
    [p {plSteps = ss} | ss <- shrinkList shrinkStep (plSteps p)]
      ++ [p {plBranch = cs} | cs <- shrinkList shrinkCommitted (plBranch p)]
      ++ [p {plBase = cs} | cs <- shrinkList shrinkCommitted (plBase p)]

shrinkStep :: Step -> [Step]
shrinkStep s =
  [s {stpBranch = as} | as <- shrinkList shrinkAction (stpBranch s)]
    ++ [s {stpBase = as} | as <- shrinkList shrinkAction (stpBase s)]

-- | A milder action to try in one's place. 'Keep' is the mildest there is, and
-- everything that falls to a commit shrinks towards it. A commit put in shrinks
-- only in what it carries: taking it out of the branch altogether is what
-- 'shrinkList' is already doing.
shrinkAction :: Action -> [Action]
shrinkAction Keep = []
shrinkAction Drop = [Keep]
shrinkAction (Rework d) = Keep : map Rework (shrinkCommitted d)
shrinkAction (Insert d) = map Insert (shrinkCommitted d)

-- | A smaller thing for a version to commit. The line has to stay printable,
-- which the 'String' shrinker does not promise; a 'RangeDiff.CommitMessage'
-- keeps itself in range.
shrinkCommitted :: Committed -> [Committed]
shrinkCommitted c =
  [c {cdMessage = m} | m <- shrink (cdMessage c)]
    ++ [c {cdLine = l} | l <- shrink (cdLine c), all isPrint l]

-- | The same scenario with every action that has nothing to fall to taken out.
--
-- Shrinking pulls at one part of a plan at a time, so a candidate can go on
-- saying what becomes of a commit that another pull took away: drop a commit
-- the branch starts out with, or a commit some rewrite put in, and every
-- rewrite that acted on it is left saying one thing too many. This is what
-- makes a candidate a scenario again.
normalize :: Plan -> Plan
normalize p = p {plSteps = go (Shape (plBase p) (plBranch p)) (plSteps p)}
  where
    go _ [] = []
    go sh (s : ss) = s' : go (outcome sh s') ss
      where
        s' = Step (trim (shBase sh) (stpBase s)) (trim (shBranch sh) (stpBranch s))

-- | A stretch's actions with the ones past its last commit dropped. A commit
-- put in stays where it was: it is the one action that doesn't fall to a commit
-- that is already there.
trim :: [Committed] -> [Action] -> [Action]
trim _ [] = []
trim cs (Insert d : as) = Insert d : trim cs as
trim [] (_ : as) = trim [] as
trim (_ : cs) (a : as) = a : trim cs as

-- | A plan as a failing case should read: the version the branch starts out as,
-- and then what every rewrite does to the stretch under the fork point and to
-- the stretch above it, bottom commit first.
instance Show Plan where
  show p =
    intercalate "\n" $
      "initial"
        : stretch "base" (map put (plBase p))
        ++ stretch "branch" (map put (plBranch p))
        ++ concat
          [ ("rewrite " ++ show i)
              : stretch "base" (map does (stpBase s))
              ++ stretch "branch" (map does (stpBranch s))
          | (i, s) <- zip [1 :: Int ..] (plSteps p)
          ]
    where
      stretch what [] = ["  " ++ what ++ ": nothing"]
      stretch what as = ("  " ++ what ++ ":") : map ("    " ++) as

      does Keep = "keep"
      does Drop = "drop"
      does (Rework d) = "rework into " ++ says d
      does (Insert d) = put d

      put d = "insert " ++ says d
      says d = show (RangeDiff.messageText (cdMessage d)) ++ " / " ++ show (cdLine d)
