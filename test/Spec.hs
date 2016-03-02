import Crow
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
