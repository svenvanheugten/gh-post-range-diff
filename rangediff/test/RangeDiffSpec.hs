-- | Reading the range-diff of one rewrite of a branch: build a branch in a real
-- repo, rewrite it, take `git range-diff` over the two versions, and check that
-- it reads back as what the rewrite did.
module RangeDiffSpec (spec) where

import RangeDiff (Change (..), Commit (..), CommitMessage (..), commitMessage, interdiff)
import RangeDiff.CommitSha (CommitSha)
import RangeDiff.Test.Repo
import Test.Hspec

spec :: Spec
spec = do
  describe "commitMessage" $
    it "keeps only the subject of a whole message" $
      messageText (commitMessage "Add the widget\n\nIt times out.\n") `shouldBe` "Add the widget"

  describe "rangeDiff" $ inRepos $ do
    it "reads a commit rebased onto a base that moved as unchanged, under its new sha" $ \repo -> do
      root <- initial repo
      [w] <- stack repo root [addWidget "30"]
      old <- version repo root w
      base <- new repo root (Patch "Move the base" [("base.txt", "moved\n")])
      rebase repo w base
      now <- version repo base w
      w' <- sha repo w
      rangeDiff repo old now `shouldRead` [unchanged w' "Add the widget"]

    it "reads a commit whose base was rewritten under it as unchanged, under its new sha" $ \repo -> do
      root <- initial repo
      [base, w] <- stack repo root [Patch "Lay the base" [("base.txt", "laid\n")], addWidget "30"]
      old <- version repo base w
      amend repo base [("base.txt", "relaid\n")]
      now <- version repo base w
      w' <- sha repo w
      rangeDiff repo old now `shouldRead` [unchanged w' "Add the widget"]

    it "reads a reworded commit under the message it had, with a metadata hunk" $ \repo -> do
      root <- initial repo
      [w] <- stack repo root [addWidget "30"]
      old <- version repo root w
      reword repo w "Add a widget"
      now <- version repo root w
      w' <- sha repo w
      rangeDiff repo old now
        `shouldRead` [updated (rewordedWidget "Add the widget" "Add a widget") w' "Add the widget"]

    it "reads an amended commit with a hunk over its patch" $ \repo -> do
      root <- initial repo
      [w] <- stack repo root [addWidget "30"]
      old <- version repo root w
      amend repo w [("widget.py", widget "5")]
      now <- version repo root w
      w' <- sha repo w
      rangeDiff repo old now `shouldRead` [updated timeoutTo5 w' "Add the widget"]

    it "reads the commit after an interdiff as a commit of its own" $ \repo -> do
      root <- initial repo
      [w, b] <- stack repo root [addWidget "30", addBenchmark]
      old <- version repo root b
      amend repo w [("widget.py", widget "5")]
      now <- version repo root b
      [w', b'] <- shas repo [w, b]
      rangeDiff repo old now
        `shouldRead` [updated timeoutTo5 w' "Add the widget", unchanged b' "Add a benchmark script"]

    it "reads a reworded and amended commit with both hunks, metadata first" $ \repo -> do
      root <- initial repo
      [w] <- stack repo root [addWidget "30"]
      old <- version repo root w
      reword repo w "Add a widget"
      amend repo w [("widget.py", widget "5")]
      now <- version repo root w
      w' <- sha repo w
      rangeDiff repo old now
        `shouldRead` [updated (rewordedWidget "Add the widget" "Add a widget" ++ timeoutTo5) w' "Add the widget"]

    it "reads a reworded body as a hunk over the message, keeping its blank lines" $ \repo -> do
      root <- initial repo
      [w] <- stack repo root [(addWidget "30") {ptMessage = "Add the widget\n\nIt times out.\n\nAfter 30s."}]
      old <- version repo root w
      reword repo w "Add the widget\n\nIt times out.\n\nAfter 5s."
      now <- version repo root w
      w' <- sha repo w
      rangeDiff repo old now
        `shouldRead` [ updated
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
                         w'
                         "Add the widget"
                     ]

    it "reads a removed commit under its old sha, and an added one under its new sha" $ \repo -> do
      root <- initial repo
      [w, b] <- stack repo root [addWidget "30", addBenchmark]
      old <- version repo root b
      bOld <- sha repo b
      abandon repo b
      t <- insertAfter repo w addTest
      now <- version repo root t
      [w', t'] <- shas repo [w, t]
      rangeDiff repo old now
        `shouldRead` [unchanged w' "Add the widget", removed bOld "Add a benchmark script", added t' "Test the widget"]

    it "reads reordered commits in the order the new version has them" $ \repo -> do
      root <- initial repo
      [w, b, t] <- stack repo root [addWidget "30", addBenchmark, addTest]
      old <- version repo root t
      moveBefore repo t w
      now <- version repo root b
      [w', b', t'] <- shas repo [w, b, t]
      rangeDiff repo old now
        `shouldRead` [unchanged t' "Test the widget", unchanged w' "Add the widget", unchanged b' "Add a benchmark script"]

    it "reads ranges of ten commits, which git pads, with an interdiff in the middle" $ \repo -> do
      root <- initial repo
      cs <- stack repo root (map numbered [1 .. 10])
      old <- version repo root (last cs)
      let four = cs !! 3
      amend repo four [("f4.txt", numberedContents 4 ++ "changed\n")]
      now <- version repo root (last cs)
      cs' <- shas repo cs
      let fourChanged = ["@@ f4.txt (new)", " +file 4, line 3", " +file 4, line 4", " +file 4, line 5", "++changed"]
          readAs 4 s = updated fourChanged s "Number 4"
          readAs n s = unchanged s ("Number " ++ show n)
      rangeDiff repo old now `shouldRead` zipWith readAs [1 :: Int ..] cs'

    it "reads removed and added commits in ranges of ten, a run of added ones among them" $ \repo -> do
      root <- initial repo
      cs <- stack repo root (map numbered [1 .. 10])
      old <- version repo root (last cs)
      tenOld <- sha repo (last cs)
      abandon repo (last cs)
      more <- stack repo (last (init cs)) [numbered 11, numbered 12]
      now <- version repo root (last more)
      nine <- shas repo (init cs)
      [eleven, twelve] <- shas repo more
      rangeDiff repo old now
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
      w' <- sha repo w
      rangeDiff repo old now `shouldRead` [updated timeoutTo5 w' "Add the widget"]

    it "keeps a subject verbatim, spaces, colons and markers included" $ \repo -> do
      root <- initial repo
      let subject = "fix(cli):  keep  `--timeout`  1:  abc ! 2:  def"
      [w] <- stack repo root [(addWidget "30") {ptMessage = subject}]
      old <- version repo root w
      amend repo w [("widget.py", widget "5")]
      now <- version repo root w
      w' <- sha repo w
      rangeDiff repo old now `shouldRead` [updated timeoutTo5 w' subject]

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

-- | A commit git paired across the two versions, with the interdiff it
-- printed under it, de-indented.
updated :: [String] -> CommitSha -> String -> Commit
updated hunk = entry (Updated (interdiff (unlines hunk)))

entry :: Change -> CommitSha -> String -> Commit
entry change s message = Commit change s (commitMessage message)

-- | The hunk git prints over the metadata of a reworded widget commit: the
-- author above the message, and the head of its patch below.
rewordedWidget :: String -> String -> [String]
rewordedWidget was now =
  [ "@@ Metadata",
    " Author: t <t@t>",
    " ",
    "  ## Commit message ##",
    "-    " ++ was,
    "+    " ++ now,
    " ",
    "  ## widget.py (new) ##",
    " @@"
  ]

-- | The hunk git prints over the patch of a widget commit amended from a
-- timeout of 30 to one of 5.
timeoutTo5 :: [String]
timeoutTo5 =
  [ "@@ widget.py (new)",
    " +class Widget:",
    " +    def __init__(self, name):",
    " +        self.name = name",
    "-+        self.timer = Timer(30)",
    "++        self.timer = Timer(5)"
  ]
