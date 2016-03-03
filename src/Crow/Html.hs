{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Crow.Html
    where

import Crow
import Lucid
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)

renderCrow :: Crow -> Html ()
renderCrow crow = 
    let rows = zipGridWithCoords $ grid crow 
        renderCell :: (Cell, Coord) -> Html ()
        renderCell (Black, _) = td_ [class_ "black"] "#"
        renderCell (cell, coord@(row', col')) =
            let lights = getLightsForCoord crow coord
                headLight :: Maybe Int
                headLight = headLightNum coord lights

                letter, number :: Maybe (Html ())
                letter = Just (toHtml . stringify $ cell)
                number = headLight >>= \i -> return $ (div_ [class_ "head"] (toHtml . show $ i))

                contents_ :: [Html ()]
                contents_ = catMaybes [number, letter]

                contents :: Html ()
                contents = sequence $ contents_

            in td_ [class_ "white"] contents
    in table_ $ 
        mapM_ (\row -> tr_ $ mapM_ renderCell row) rows
