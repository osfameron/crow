{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Lucid

import qualified Crow as C
import Crow.Html

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop index <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory "static")

index :: Snap ()
index =
    let crow = C.parseGrid2Crow cw
        output = html_ $ do
            title_ "Crow"
            link_ [rel_ "stylesheet", href_ "static/crow.css"]
            renderCrow crow
    in writeLBS .renderBS $ output

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

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
