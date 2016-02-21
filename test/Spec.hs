import Crow
import Test.Hspec
import Data.List

cw :: [String]
cw = 
    [ "      #        "
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

    describe "CrowCell" $ do
        it "can stringify a crow-cell" $ do
            let (Crow ((cc:_) : _)) = crow
            (stringify cc) `shouldBe` "-"

    describe "Light" $ do
        it "can stringify a light" $ do
            let (Crow ((cc:_) : _)) = crow
                light = (lights cc) !! 0
            (stringify light) `shouldBe` "1 Across '      ' (6)"

