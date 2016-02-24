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

    describe "Crow" $ do
        it "can parse a grid" $ do
            (stringify crow) `shouldBe` expected

    describe "Light" $ do
        it "can stringify a light" $ do
            let light = (lights crow) !! 0
            (stringify light) `shouldBe` "1 Across (6)"

        it "can stringOnGrid a light" $ do
            let light = (lights crow) !! 0
            (stringOnGrid light (grid crow)) `shouldBe` "LEMONS"


    describe "longestMatch" $ do
        let m = "watchtower"
        let ss = ["watch", "watchtower", "tower", "lemon"]
        it "returns a unique match" $ do
            longestMatch ss "watch" `shouldBe` (Just ["watch"])
        it "returns longest match" $ do
            longestMatch ss "watchtower" `shouldBe` (Just ["watchtower"])
        it "matches 2 items" $ do
            longestMatch ss "towerwatch" `shouldBe` (Just ["tower", "watch"])
        it "matches 3 items" $ do
            longestMatch ss "lemonwatchtowerlemon" `shouldBe` (Just ["lemon", "watchtower", "lemon"])
        it "won't match a nonexistent word" $ do
            longestMatch ss "orange" `shouldBe` Nothing
        it "won't partially match" $ do
            longestMatch ss "watchable" `shouldBe` Nothing
