module RangeDiffSpec (spec) where

import RangeDiff (CommitMessage (..), Shape (..), commitMessage, interdiff, interdiffLines)
import Test.Hspec

spec :: Spec
spec = do
  describe "commitMessage" $
    it "keeps only the subject of a whole message" $
      messageText (commitMessage "Add the widget\n\nIt times out.\n") `shouldBe` "Add the widget"

  describe "interdiffLines" $ do
    it "splits a line of a diff of patches after both its markers" $
      interdiffLines
        ( interdiff
            DiffOfPatches
            ( unlines
                [ "@@ widget.py (new)",
                  " +class Widget:",
                  "-+        self.timer = Timer(30)",
                  "++        self.timer = Timer(5)"
                ]
            )
        )
        `shouldBe` [ ("", "@@ widget.py (new)"),
                     (" +", "class Widget:"),
                     ("-+", "        self.timer = Timer(30)"),
                     ("++", "        self.timer = Timer(5)")
                   ]

    it "splits a line of a diff of patches that isn't a patch's after the one marker" $
      interdiffLines
        ( interdiff
            DiffOfPatches
            ( unlines
                [ "@@ Metadata",
                  " Author: t <t@t>",
                  " @@ class Widget:"
                ]
            )
        )
        `shouldBe` [ ("", "@@ Metadata"),
                     (" ", "Author: t <t@t>"),
                     (" ", "@@ class Widget:")
                   ]

    it "splits a line of a diff of commits after its one marker, but a header's after none" $
      interdiffLines
        ( interdiff
            DiffOfCommits
            ( unlines
                [ "diff --git a/widget.py b/widget.py",
                  "--- a/widget.py",
                  "+++ b/widget.py",
                  "@@ -3,4 +3,4 @@",
                  " class Widget:",
                  "-        self.timer = Timer(30)",
                  "+        self.timer = Timer(5)",
                  "\\ No newline at end of file",
                  "diff --git a/README.md b/README.md",
                  "new file mode 100644",
                  "--- /dev/null",
                  "+++ b/README.md",
                  "@@ -0,0 +1,1 @@",
                  "+# Widget"
                ]
            )
        )
        `shouldBe` [ ("", "diff --git a/widget.py b/widget.py"),
                     ("", "--- a/widget.py"),
                     ("", "+++ b/widget.py"),
                     ("", "@@ -3,4 +3,4 @@"),
                     (" ", "class Widget:"),
                     ("-", "        self.timer = Timer(30)"),
                     ("+", "        self.timer = Timer(5)"),
                     ("", "\\ No newline at end of file"),
                     ("", "diff --git a/README.md b/README.md"),
                     ("", "new file mode 100644"),
                     ("", "--- /dev/null"),
                     ("", "+++ b/README.md"),
                     ("", "@@ -0,0 +1,1 @@"),
                     ("+", "# Widget")
                   ]

    it "keeps the marker of a line of a hunk that reads like a header" $
      interdiffLines
        ( interdiff
            DiffOfCommits
            ( unlines
                [ "diff --git a/Widget.hs b/Widget.hs",
                  "--- a/Widget.hs",
                  "+++ b/Widget.hs",
                  "@@ -1,2 +1,2 @@",
                  "--- a/b is how a widget is named",
                  "+++ counts the widgets"
                ]
            )
        )
        `shouldBe` [ ("", "diff --git a/Widget.hs b/Widget.hs"),
                     ("", "--- a/Widget.hs"),
                     ("", "+++ b/Widget.hs"),
                     ("", "@@ -1,2 +1,2 @@"),
                     ("-", "-- a/b is how a widget is named"),
                     ("+", "++ counts the widgets")
                   ]
