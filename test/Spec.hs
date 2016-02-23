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

