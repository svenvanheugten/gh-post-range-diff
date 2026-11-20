-- | The range-diff `git range-diff` takes. Besides the shared 'examples', it
-- has examples of its own: about how git pairs commits up, which it does by
-- how alike their patches are, and about reading the text git prints, which a
-- repo's config can change.
module RangeDiff.GitSpec (spec) where

import RangeDiff (Shape (..))
import RangeDiff.Git qualified
import RangeDiff.Test.Examples
import RangeDiff.Test.Repo
import Test.Hspec

spec :: Spec
spec = describe "git" $ inRepos $ do
  examples way

  it "reads a commit rewritten past recognition as one removed and one added" $ \repo -> do
    (old, now, wOld, w') <- rewrittenPastRecognition repo
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [removed wOld "Add the widget", added w' "Add the widget"]

  it "reads ranges of ten commits, which git pads, with an interdiff in the middle" $ \repo -> do
    root <- initial repo
    cs <- stack repo root (map numbered [1 .. 10])
    old <- version repo root (last cs)
    let four = cs !! 3
    amend repo four [("f4.txt", numberedContents 4 ++ "changed\n")]
    now <- version repo root (last cs)
    cs' <- shas repo cs
    let fourChanged = ["@@ f4.txt (new)", " +file 4, line 3", " +file 4, line 4", " +file 4, line 5", "++changed"]
        readAs 4 s = updated way fourChanged s "Number 4"
        readAs n s = unchanged s ("Number " ++ show n)
    rangeDiff (wayHandle way) repo old now `shouldRead` zipWith readAs [1 :: Int ..] cs'

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
