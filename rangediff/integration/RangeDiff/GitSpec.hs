-- | The range-diff `git range-diff` takes. Besides the shared 'examples', and
-- the 'gitPairing' ones about how git pairs commits up, which it does by how
-- alike their patches are, it has examples of its own about reading the text
-- git prints.
module RangeDiff.GitSpec (spec) where

import RangeDiff (Shape (..))
import RangeDiff.Git qualified
import RangeDiff.Test.Examples
import RangeDiff.Test.Repo
import Test.Hspec

spec :: Spec
spec = describe "git" $ inRepos $ do
  examples way
  gitPairing way

  it "reads removed and added commits in ranges of ten, a run of added ones among them" $ \repo -> do
    root <- initial repo
    cs <- stack repo root (map numbered [1 .. 10])
    old <- version repo root (last cs)
    [tenOld] <- shas repo [last cs]
    abandon repo (last cs)
    more <- stack repo (last (init cs)) [numbered 11, numbered 12]
    now <- version repo root (last more)
    nine <- shas repo (init cs)
    [eleven, twelve] <- shas repo more
    rangeDiff (wayHandle way) repo old now
      `shouldRead` ( [unchanged s ("Number " ++ show n) | (n, s) <- zip [1 :: Int ..] nine]
                       ++ [removed tenOld "Number 10", added eleven "Number 11", added twelve "Number 12"]
                   )

  it "reads a range-diff in a repo that has git colour it" $ \repo -> do
    configure repo "color.ui" "always"
    root <- initial repo
    [w] <- stack repo root [addWidget "30"]
    old <- version repo root w
    amend repo w [("widget.py", widget "5")]
    now <- version repo root w
    [w'] <- shas repo [w]
    rangeDiff (wayHandle way) repo old now `shouldRead` [updated way (wayAmended way) w' "Add the widget"]

  it "reads a commit reworded just below its subject under the subject it kept" $ \repo -> do
    root <- initial repo
    [w] <- stack repo root [(addWidget "30") {ptMessage = "Add the widget\n\nIt times out."}]
    old <- version repo root w
    reword repo w "Add the widget\n\nIt hangs."
    now <- version repo root w
    [w'] <- shas repo [w]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [ updated
                       way
                       [ "@@ Metadata",
                         "  ## Commit message ##",
                         "     Add the widget",
                         " ",
                         "-    It times out.",
                         "+    It hangs.",
                         " ",
                         "  ## widget.py (new) ##",
                         " @@"
                       ]
                       w'
                       "Add the widget"
                   ]

  it "reads a reworded commit under the message it was reworded to in a repo that has git diff without context" $ \repo -> do
    configure repo "diff.context" "0"
    root <- initial repo
    [w] <- stack repo root [addWidget "30"]
    old <- version repo root w
    reword repo w "Add a widget"
    now <- version repo root w
    [w'] <- shas repo [w]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [updated way (wayReworded way) w' "Add a widget"]

  it "reads a merge whose conflict was resolved anew as one removed and one added" $ \repo -> do
    (base, w, r, m) <- mergedWithConflict repo
    old <- version repo base m
    mOld <- sha repo m
    amend repo m [("retries.txt", retriesContents "4")]
    now <- version repo base m
    [w', r', m'] <- shas repo [w, r, m]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [ unchanged w' "Add the widget",
                     unchanged r' "Retry 2 times",
                     removed mOld "Merge the base",
                     added m' "Merge the base"
                   ]

  it "reads a merge that resolved a conflict as one removed and one added when a commit below it was amended" $ \repo -> do
    (base, w, r, m) <- mergedWithConflict repo
    old <- version repo base m
    mOld <- sha repo m
    amend repo w [("widget.py", widget "5")]
    now <- version repo base m
    [w', r', m'] <- shas repo [w, r, m]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [ updated way (wayAmended way) w' "Add the widget",
                     unchanged r' "Retry 2 times",
                     removed mOld "Merge the base",
                     added m' "Merge the base"
                   ]

-- | `git range-diff`, which diffs the two commits' printed patches: a reword
-- is a hunk over the metadata git prints above a patch, and an amend a hunk
-- over the patch, each line of it still carrying the patch's own +/-.
way :: Way
way =
  Way
    { wayHandle = RangeDiff.Git.git,
      wayShape = DiffOfPatches,
      wayReworded =
        [ "@@ Metadata",
          " Author: t <t@t>",
          " ",
          "  ## Commit message ##",
          "-    Add the widget",
          "+    Add a widget",
          " ",
          "  ## widget.py (new) ##",
          " @@"
        ],
      wayAmended =
        [ "@@ widget.py (new)",
          " +class Widget:",
          " +    def __init__(self, name):",
          " +        self.name = name",
          "-+        self.timer = Timer(30)",
          "++        self.timer = Timer(5)"
        ],
      wayBodyReworded =
        [ "@@ Commit message",
          " ",
          "     It times out.",
          " ",
          "-    After 30s.",
          "+    After 5s.",
          " ",
          "  ## widget.py (new) ##",
          " @@"
        ]
    }
