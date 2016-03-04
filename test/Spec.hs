import Crow
import Crow.Html
import Test.Hspec
import Data.List

cw :: [String]
cw = 
    [ "LEMONS#        "
    , "# # # # # # # #"
    , "        #      "
    , "# # # # # # # #"
    , "###            "
    , "# # # # # ### #"
    , "    ###        "
    , "# # # # # # # #"
    , "        ###    "
    , "# ### # # # # #"
    , "            ###"
    , "# # # # # # # #"
    , "      #        "
    , "# # # # # # # #"
    , "        #      "
    ]

expected :: String
expected = intercalate "\n"
    [ "-+-+-+#+-+-+-+-"
    , "#|#|#|#|#|#|#|#"
    , "-+-+-+-+#+-+-+-"
    , "#|#|#|#|#|#|#|#"
    , "###+-+-+-+-+-+-"
    , "#|#|#|#|#|###|#"
    , "-+-+###+-+-+-+-"
    , "#|#|#|#|#|#|#|#"
    , "-+-+-+-+###+-+-"
    , "#|###|#|#|#|#|#"
    , "-+-+-+-+-+-+###"
    , "#|#|#|#|#|#|#|#"
    , "-+-+-+#+-+-+-+-"
    , "#|#|#|#|#|#|#|#"
    , "-+-+-+-+#+-+-+-"
    ]

main :: IO ()
main = hspec $ do
    let crow = parseGrid2Crow cw
    let light1a = (lights crow) !! 0

    describe "Crow" $ do
        it "can parse a grid" $ do
            (stringify crow) `shouldBe` expected

    describe "Light" $ do
        it "can stringify a light" $ do
            (stringify light1a) `shouldBe` "1 Across (6)"

        it "can stringOnGrid a light" $ do
            (stringOnGrid (grid crow) light1a) `shouldBe` "LEMONS"


    describe "longestMatch" $ do
        let ss = ["watch", "watchtower", "tower", "lemon"]
        let lm = longestMatch ss
        it "returns a unique match" $ do
            lm "watch" `shouldBe` (Just ["watch"])
        it "returns longest match" $ do
            lm "watchtower" `shouldBe` (Just ["watchtower"])
        it "matches 2 items" $ do
            lm "towerwatch" `shouldBe` (Just ["tower", "watch"])
        it "matches 3 items" $ do
            lm "lemonwatchtowerlemon" `shouldBe` (Just ["lemon", "watchtower", "lemon"])
        it "won't match a nonexistent word" $ do
            lm "orange" `shouldBe` Nothing
        it "won't partially match" $ do
            lm "watchable" `shouldBe` Nothing

    describe "getLightsForAnswer (using longestMatch algorithm)" $ do
        let lm = getLightsForAnswer (grid crow) (lights crow)
        it "returns a unique match" $ do
            lm ["LEMONS"] `shouldBe` Just [light1a]
        it "won't match a nonexistent word" $ do
            lm ["ORANGE"] `shouldBe` Nothing

    describe "Crow.Html" $ do
        it "can render a Crow grid" $ do
            (show . renderCrow $ crow) `shouldBe` "<table><tr><td class=\"white\"><span class=\"head\">1</span><input maxlength=\"1\" value=\"L\" type=\"text\"></td><td class=\"white\"><span class=\"head\">2</span><input maxlength=\"1\" value=\"E\" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\"M\" type=\"text\"></td><td class=\"white\"><span class=\"head\">3</span><input maxlength=\"1\" value=\"O\" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\"N\" type=\"text\"></td><td class=\"white\"><span class=\"head\">4</span><input maxlength=\"1\" value=\"S\" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">5</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><span class=\"head\">6</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><span class=\"head\">7</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><span class=\"head\">8</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td></tr><tr><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td></tr><tr><td class=\"white\"><span class=\"head\">9</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">10</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td></tr><tr><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td></tr><tr><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">11</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td></tr><tr><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">12</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td></tr><tr><td class=\"white\"><span class=\"head\">13</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">14</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><span class=\"head\">15</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td></tr><tr><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">16</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td></tr><tr><td class=\"white\"><span class=\"head\">17</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">18</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td></tr><tr><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">19</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td></tr><tr><td class=\"white\"><span class=\"head\">20</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><span class=\"head\">21</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"dark\">#</td><td class=\"dark\">#</td></tr><tr><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">22</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td></tr><tr><td class=\"white\"><span class=\"head\">23</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">24</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td></tr><tr><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td></tr><tr><td class=\"white\"><span class=\"head\">25</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"dark\">#</td><td class=\"white\"><span class=\"head\">26</span><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td><td class=\"white\"><input maxlength=\"1\" value=\" \" type=\"text\"></td></tr></table>"
