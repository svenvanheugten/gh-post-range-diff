-- | The range-diff jj takes: the shared 'examples', as jj writes their
-- interdiffs, and the one jj reads its own way, from pairing commits up by
-- their change ids.
module RangeDiff.JjSpec (spec) where

import RangeDiff (Shape (..))
import RangeDiff.Jj qualified
import RangeDiff.Test.Examples
import RangeDiff.Test.Repo
import Test.Hspec

spec :: Spec
spec = describe "jj" $ inRepos $ do
  examples way

  it "pairs a commit rewritten past recognition by its change id" $ \repo -> do
    (old, now, _, w') <- rewrittenPastRecognition repo
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [ updated
                       way
                       [ "diff --git a/widget.py b/widget.py",
                         "--- a/widget.py",
                         "+++ b/widget.py",
                         "@@ -1,6 +1,3 @@",
                         "-from timer import Timer",
                         "+from gadget import Gadget",
                         " ",
                         "-class Widget:",
                         "-    def __init__(self, name):",
                         "-        self.name = name",
                         "-        self.timer = Timer(30)",
                         "+WIDGET = Gadget(\"widget\")"
                       ]
                       w'
                       "Add the widget"
                   ]

  it "reads a merge whose conflict was resolved anew with the change to its resolution" $ \repo -> do
    (base, w, r, m) <- mergedWithConflict repo
    old <- version repo base m
    amend repo m [("retries.txt", retriesContents "4")]
    now <- version repo base m
    [w', r', m'] <- shas repo [w, r, m]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [ unchanged w' "Add the widget",
                     unchanged r' "Retry 2 times",
                     updated
                       way
                       [ "diff --git a/retries.txt b/retries.txt",
                         "--- a/retries.txt",
                         "+++ b/retries.txt",
                         "@@ -1,3 +1,3 @@",
                         " # How often to retry a widget.",
                         " ",
                         "-RETRIES = 3",
                         "+RETRIES = 4"
                       ]
                       m'
                       "Merge the base"
                   ]

  it "reads a merge that resolved a conflict as unchanged when a commit below it was amended" $ \repo -> do
    (base, w, r, m) <- mergedWithConflict repo
    old <- version repo base m
    amend repo w [("widget.py", widget "5")]
    now <- version repo base m
    [w', r', m'] <- shas repo [w, r, m]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [updated way (wayAmended way) w' "Add the widget", unchanged r' "Retry 2 times", unchanged m' "Merge the base"]

-- | `jj interdiff`, which diffs the two commits themselves, in git's format: a
-- section per file the rewrite touched, with the commit's description counted
-- among them under a name no file could have.
way :: Way
way =
  Way
    { wayHandle = RangeDiff.Jj.jj,
      wayShape = DiffOfCommits,
      wayReworded =
        description
          [ "@@ -1,1 +1,1 @@",
            "-Add the widget",
            "+Add a widget"
          ],
      wayAmended =
        [ "diff --git a/widget.py b/widget.py",
          "--- a/widget.py",
          "+++ b/widget.py",
          "@@ -3,4 +3,4 @@",
          " class Widget:",
          "     def __init__(self, name):",
          "         self.name = name",
          "-        self.timer = Timer(30)",
          "+        self.timer = Timer(5)"
        ],
      wayBodyReworded =
        description
          [ "@@ -2,4 +2,4 @@",
            " ",
            " It times out.",
            " ",
            "-After 30s.",
            "+After 5s."
          ],
      wayLeavesOutMerges = False
    }
  where
    -- jj names the description bare, without the a/ and b/ it puts on a file.
    description hunk =
      [ "diff --git a/JJ-COMMIT-DESCRIPTION b/JJ-COMMIT-DESCRIPTION",
        "--- JJ-COMMIT-DESCRIPTION",
        "+++ JJ-COMMIT-DESCRIPTION"
      ]
        ++ hunk
