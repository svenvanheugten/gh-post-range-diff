-- | Reading the range-diff of one rewrite of a branch: build a branch in a real
-- repo, rewrite it, take a range-diff over the two versions, and check that it
-- reads back as what the rewrite did.
--
-- There are two ways to take one, and the same examples hold of both. All they
-- differ on is how a rewritten commit's interdiff is written, which is what a
-- 'Way' says, and which commits they pair up: git guesses that from how alike
-- two patches are, and jj reads it off the change ids.
module RangeDiffSpec (spec) where

import RangeDiff (Change (..), Commit (..), CommitMessage (..), Handle, commitMessage, interdiff)
import RangeDiff.CommitSha (CommitSha)
import RangeDiff.Git qualified
import RangeDiff.Jj qualified
import RangeDiff.Test.Repo
import Test.Hspec

-- | One way of taking a range-diff, and how it writes the interdiffs the
-- examples come out with.
data Way = Way
  { wayHandle :: FilePath -> Handle,
    -- | the shas it reports the commits of some changes under
    wayShas :: Repo -> [ChangeId] -> IO [CommitSha],
    -- | the widget commit reworded from "Add the widget" to "Add a widget"
    wayReworded :: [String],
    -- | the widget commit amended from a timeout of 30 to one of 5
    wayAmended :: [String],
    -- | the last line of the widget commit's message body changed from "After
    -- 30s." to "After 5s."
    wayBodyReworded :: [String]
  }

spec :: Spec
spec = do
  describe "commitMessage" $
    it "keeps only the subject of a whole message" $
      messageText (commitMessage "Add the widget\n\nIt times out.\n") `shouldBe` "Add the widget"

  describe "RangeDiff.Git.rangeDiff" $ inRepos $ do
    examples git

    it "reads a commit rewritten past recognition as one removed and one added" $ \repo -> do
      (old, now, wOld, w') <- rewrittenPastRecognition git repo
      rangeDiff (wayHandle git) repo old now
        `shouldRead` [removed wOld "Add the widget", added w' "Add the widget"]

    it "reads ranges of ten commits, which git pads, with an interdiff in the middle" $ \repo -> do
      root <- initial repo
      cs <- stack repo root (map numbered [1 .. 10])
      old <- version repo root (last cs)
      let four = cs !! 3
      amend repo four [("f4.txt", numberedContents 4 ++ "changed\n")]
      now <- version repo root (last cs)
      cs' <- wayShas git repo cs
      let fourChanged = ["@@ f4.txt (new)", " +file 4, line 3", " +file 4, line 4", " +file 4, line 5", "++changed"]
          readAs 4 s = updated fourChanged s "Number 4"
          readAs n s = unchanged s ("Number " ++ show n)
      rangeDiff (wayHandle git) repo old now `shouldRead` zipWith readAs [1 :: Int ..] cs'

    it "reads removed and added commits in ranges of ten, a run of added ones among them" $ \repo -> do
      root <- initial repo
      cs <- stack repo root (map numbered [1 .. 10])
      old <- version repo root (last cs)
      [tenOld] <- wayShas git repo [last cs]
      abandon repo (last cs)
      more <- stack repo (last (init cs)) [numbered 11, numbered 12]
      now <- version repo root (last more)
      nine <- wayShas git repo (init cs)
      [eleven, twelve] <- wayShas git repo more
      rangeDiff (wayHandle git) repo old now
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
      [w'] <- wayShas git repo [w]
      rangeDiff (wayHandle git) repo old now `shouldRead` [updated (wayAmended git) w' "Add the widget"]

    it "reads a commit reworded just below its subject under the subject it kept" $ \repo -> do
      root <- initial repo
      [w] <- stack repo root [(addWidget "30") {ptMessage = "Add the widget\n\nIt times out."}]
      old <- version repo root w
      reword repo w "Add the widget\n\nIt hangs."
      now <- version repo root w
      [w'] <- wayShas git repo [w]
      rangeDiff (wayHandle git) repo old now
        `shouldRead` [ updated
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
      [w'] <- wayShas git repo [w]
      rangeDiff (wayHandle git) repo old now
        `shouldRead` [updated (wayReworded git) w' "Add a widget"]

  describe "RangeDiff.Jj.rangeDiff" $ inRepos $ do
    examples jj

    it "pairs a commit rewritten past recognition by its change id" $ \repo -> do
      (old, now, _, w') <- rewrittenPastRecognition jj repo
      rangeDiff (wayHandle jj) repo old now
        `shouldRead` [ updated
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

-- | What either way has to read the same, but for how it writes an interdiff.
examples :: Way -> SpecWith Repo
examples way = do
  it "reads a commit rebased onto a base that moved as unchanged, under its new sha" $ \repo -> do
    root <- initial repo
    [w] <- stack repo root [addWidget "30"]
    old <- version repo root w
    base <- new repo root (Patch "Move the base" [("base.txt", "moved\n")])
    rebase repo w base
    now <- version repo base w
    [w'] <- wayShas way repo [w]
    rangeDiff (wayHandle way) repo old now `shouldRead` [unchanged w' "Add the widget"]

  it "reads a commit whose base was rewritten under it as unchanged, under its new sha" $ \repo -> do
    root <- initial repo
    [base, w] <- stack repo root [Patch "Lay the base" [("base.txt", "laid\n")], addWidget "30"]
    old <- version repo base w
    amend repo base [("base.txt", "relaid\n")]
    now <- version repo base w
    [w'] <- wayShas way repo [w]
    rangeDiff (wayHandle way) repo old now `shouldRead` [unchanged w' "Add the widget"]

  it "reads a reworded commit under the message it was reworded to" $ \repo -> do
    root <- initial repo
    [w] <- stack repo root [addWidget "30"]
    old <- version repo root w
    reword repo w "Add a widget"
    now <- version repo root w
    [w'] <- wayShas way repo [w]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [updated (wayReworded way) w' "Add a widget"]

  it "reads an amended commit with the change to its file" $ \repo -> do
    root <- initial repo
    [w] <- stack repo root [addWidget "30"]
    old <- version repo root w
    amend repo w [("widget.py", widget "5")]
    now <- version repo root w
    [w'] <- wayShas way repo [w]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [updated (wayAmended way) w' "Add the widget"]

  it "reads the commit after an interdiff as a commit of its own" $ \repo -> do
    root <- initial repo
    [w, b] <- stack repo root [addWidget "30", addBenchmark]
    old <- version repo root b
    amend repo w [("widget.py", widget "5")]
    now <- version repo root b
    [w', b'] <- wayShas way repo [w, b]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [updated (wayAmended way) w' "Add the widget", unchanged b' "Add a benchmark script"]

  it "reads a reworded and amended commit with both changes, the message first" $ \repo -> do
    root <- initial repo
    [w] <- stack repo root [addWidget "30"]
    old <- version repo root w
    reword repo w "Add a widget"
    amend repo w [("widget.py", widget "5")]
    now <- version repo root w
    [w'] <- wayShas way repo [w]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [updated (wayReworded way ++ wayAmended way) w' "Add a widget"]

  it "reads a reworded body as a change to the message, keeping its blank lines" $ \repo -> do
    root <- initial repo
    [w] <- stack repo root [(addWidget "30") {ptMessage = "Add the widget\n\nIt times out.\n\nAfter 30s."}]
    old <- version repo root w
    reword repo w "Add the widget\n\nIt times out.\n\nAfter 5s."
    now <- version repo root w
    [w'] <- wayShas way repo [w]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [updated (wayBodyReworded way) w' "Add the widget"]

  it "reads a removed commit under its old sha, and an added one under its new sha" $ \repo -> do
    root <- initial repo
    [w, b] <- stack repo root [addWidget "30", addBenchmark]
    old <- version repo root b
    [bOld] <- wayShas way repo [b]
    abandon repo b
    t <- insertAfter repo w addTest
    now <- version repo root t
    [w', t'] <- wayShas way repo [w, t]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [unchanged w' "Add the widget", removed bOld "Add a benchmark script", added t' "Test the widget"]

  it "reads reordered commits in the order the new version has them" $ \repo -> do
    root <- initial repo
    [w, b, t] <- stack repo root [addWidget "30", addBenchmark, addTest]
    old <- version repo root t
    moveBefore repo t w
    now <- version repo root b
    [w', b', t'] <- wayShas way repo [w, b, t]
    rangeDiff (wayHandle way) repo old now
      `shouldRead` [unchanged t' "Test the widget", unchanged w' "Add the widget", unchanged b' "Add a benchmark script"]

  it "keeps a subject verbatim, spaces, colons and markers included" $ \repo -> do
    root <- initial repo
    let subject = "fix(cli):  keep  `--timeout`  1:  abc ! 2:  def"
    [w] <- stack repo root [(addWidget "30") {ptMessage = subject}]
    old <- version repo root w
    amend repo w [("widget.py", widget "5")]
    now <- version repo root w
    [w'] <- wayShas way repo [w]
    rangeDiff (wayHandle way) repo old now `shouldRead` [updated (wayAmended way) w' subject]

-- | The widget commit, with its file then written over with one that has
-- nothing left in common with it: the two versions, and the shas @way@
-- reports the commit it was on and the commit it is on now under.
rewrittenPastRecognition :: Way -> Repo -> IO ((CommitSha, CommitSha), (CommitSha, CommitSha), CommitSha, CommitSha)
rewrittenPastRecognition way repo = do
  root <- initial repo
  [w] <- stack repo root [addWidget "30"]
  old <- version repo root w
  [wOld] <- wayShas way repo [w]
  amend repo w [("widget.py", unlines ["from gadget import Gadget", "", "WIDGET = Gadget(\"widget\")"])]
  now <- version repo root w
  [w'] <- wayShas way repo [w]
  pure (old, now, wOld, w')

-- * The two ways

-- | `git range-diff`, which diffs the two commits' printed patches: a reword
-- is a hunk over the metadata git prints above a patch, and an amend a hunk
-- over the patch, each line of it still carrying the patch's own +/-. A commit
-- is reported under its sha as git abbreviates it.
git :: Way
git =
  Way
    { wayHandle = RangeDiff.Git.git,
      wayShas = \repo cs -> abbreviated repo =<< shas repo cs,
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

-- | `jj interdiff`, which diffs the two commits themselves, in git's format: a
-- section per file the rewrite touched, with the commit's description counted
-- among them under a name no file could have. A commit is reported under its
-- sha in full.
jj :: Way
jj =
  Way
    { wayHandle = RangeDiff.Jj.jj,
      wayShas = shas,
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
          ]
    }
  where
    -- jj names the description bare, without the a/ and b/ it puts on a file.
    description hunk =
      [ "diff --git a/JJ-COMMIT-DESCRIPTION b/JJ-COMMIT-DESCRIPTION",
        "--- JJ-COMMIT-DESCRIPTION",
        "+++ JJ-COMMIT-DESCRIPTION"
      ]
        ++ hunk

-- | A version of the branch, as a range-diff is taken over it: where it forks
-- off, and its tip, as the repo stands.
version :: Repo -> ChangeId -> ChangeId -> IO (CommitSha, CommitSha)
version repo base tip = do
  [b, t] <- shas repo [base, tip]
  pure (b, t)

-- * The commits the examples are built from

addWidget :: String -> Patch
addWidget timeout = Patch "Add the widget" [("widget.py", widget timeout)]

widget :: String -> String
widget timeout =
  unlines
    [ "from timer import Timer",
      "",
      "class Widget:",
      "    def __init__(self, name):",
      "        self.name = name",
      "        self.timer = Timer(" ++ timeout ++ ")"
    ]

addBenchmark :: Patch
addBenchmark =
  Patch
    "Add a benchmark script"
    [("bench.py", unlines ["from widget import Widget", "", "for i in range(1000):", "    Widget(str(i))"])]

addTest :: Patch
addTest =
  Patch
    "Test the widget"
    [("test_widget.py", unlines ["from widget import Widget", "", "def test_name():", "    assert Widget(\"w\").name == \"w\""])]

-- | The @n@th of a run of commits, each writing a file of its own.
numbered :: Int -> Patch
numbered n = Patch ("Number " ++ show n) [("f" ++ show n ++ ".txt", numberedContents n)]

numberedContents :: Int -> String
numberedContents n = unlines ["file " ++ show n ++ ", line " ++ show k | k <- [1 .. 5 :: Int]]

-- * What a range-diff should read as

unchanged, added, removed :: CommitSha -> String -> Commit
unchanged = entry Unchanged
added = entry Added
removed = entry Removed

-- | A commit paired across the two versions, with the interdiff printed under
-- it, de-indented.
updated :: [String] -> CommitSha -> String -> Commit
updated hunk = entry (Updated (interdiff (unlines hunk)))

entry :: Change -> CommitSha -> String -> Commit
entry change s message = Commit change s (commitMessage message)
