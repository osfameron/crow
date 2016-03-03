{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Crow.Html
    where

import Crow
import Lucid

renderCrow :: Crow -> Html ()
renderCrow c = 
    let rows = zipGridWithCoords $ grid c 
        renderCell (Black, _) = return td_ [class_ "black"] "#"
        renderCell (cell, (row', col')) =
            do
                let letter = toHtml . stringify $ cell

                return td_ [class_ "white"] letter
    in table_ $ 
        mapM_ (\row -> tr_ $ mapM renderCell row) rows
