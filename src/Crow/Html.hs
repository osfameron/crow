{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Crow.Html
    ( renderCrow
    , renderText
    )
    where

import Crow
import Lucid
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.Text

renderCrow :: Crow -> Html ()
renderCrow crow = 
    let rows = zipGridWithCoords $ grid crow 
        renderCell :: (Cell, Coord) -> Html ()
        renderCell (Black, _) = td_ [class_ "dark"] "#"
        renderCell (cell, coord@(row', col')) =
            let lights = getLightsForCoord crow coord
                headLight :: Maybe Int
                headLight = headLightNum coord lights

                letter, number :: Maybe (Html ())
                letter = Just $ input_ [value_ (pack . stringify $ cell), type_ "text", maxlength_ "1"]
                number = headLight >>= \i -> return $ (span_ [class_ "head"] (toHtml . show $ i))

                contents :: Html ()
                contents = sequence_ . catMaybes $ [number, letter]

            in td_ [class_ "white"] contents
    in table_ $ 
        mapM_ (\row -> tr_ $ mapM_ renderCell row) rows
