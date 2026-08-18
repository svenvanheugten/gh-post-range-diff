module GhPostRangeDiff.RenderSpec (spec) where

import Data.List (intercalate)
import GhPostRangeDiff.RangeDiff (Change (..), Commit (..))
import GhPostRangeDiff.Render (format)
import Test.Hspec

-- Status emojis, as the codepoints 'format' emits.
green, red, orange, white :: String
green = "\128994" -- 🟢 added
red = "\128308" -- 🔴 removed
orange = "\128992" -- 🟠 updated
white = "\9898" -- ⚪ unchanged

-- An interdiff whose changed line carries a ``` run, so the fence has to widen
-- to four backticks to stay unbreakable.
interdiff :: [String]
interdiff = ["@@ big.txt (new)", " +l8", "-+```OLD", "++```NEW"]

spec :: Spec
spec = describe "format" $ do
  it "renders a status marker and a bare sha per commit, blank-line separated" $
    format
      [ Commit Unchanged "1111111" "add keep",
        Commit Removed "2222222" "add gone",
        Commit Added "3333333" "add fresh"
      ]
      `shouldBe` intercalate
        "\n"
        [ white ++ " **Unchanged** 1111111 add keep",
          "",
          red ++ " **Removed** 2222222 add gone",
          "",
          green ++ " **Added** 3333333 add fresh"
        ]

  it "shows a changed commit's interdiff in-place, in a fence that survives backticks in the patch" $
    format [Commit (Updated interdiff) "4444444" "big feature"]
      `shouldBe` intercalate
        "\n"
        [ orange ++ " **Updated** 4444444 big feature",
          "",
          "````diff",
          "@@ big.txt (new)",
          " +l8",
          "-+```OLD",
          "++```NEW",
          "````"
        ]
