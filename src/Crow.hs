module Crow
    where

import Data.List (groupBy, sortBy, stripPrefix, transpose, intercalate)
import Data.Maybe (maybeToList, listToMaybe)
import Data.Function (on)
import qualified Data.Map as M

-- Direction
data Dir = Across | Down
    deriving (Show, Eq)

data Cell = Black | White (Maybe Char)
    deriving (Show, Eq)

type Grid = [[Cell]]
type CellCoord = (Cell, Coord)

type Coord = (Int,Int)

-- a Run of White cells, associated with a direction
data Run = Run
    { dir :: Dir
    , coords :: [Coord]
    }
    deriving (Show, Eq)

-- a numbered run (1-based), e.g. "5 Across" or "2 Down"
data Light = Light
    { lnum :: Int
    , run :: Run
    }
    deriving (Show, Eq)

-- get list of lights that a given coordinate is in
type CoordLightMap = M.Map Coord [Light]

type Answer = [String]
type Enumeration = [Int]

data Clue = Clue String Enumeration [Light]

data Crow = Crow
    { grid :: Grid
    , lights :: [Light]
    , coordLightMap :: CoordLightMap
    }
    deriving Show

-- Haskell's standard Show is great for machine-readable output, but we also want
-- human-readable for debugging.  This typeclass lets us define a 'stringify' function
class (Show a) => Stringify a where
    stringify :: a -> String
    stringify = show

parseGrid2Crow :: [String] -> Crow
parseGrid2Crow lines =
    let grid = parseGrid lines
        lights = getLights grid
        lm = getCoordLightMap lights
    in Crow grid lights lm

parseGrid :: [String] -> Grid
parseGrid = mapOverGrid parseCell

mapOverGrid = map.map

parseCell :: Char -> Cell
parseCell '#' = Black
parseCell ' ' = White Nothing
parseCell c = White $ Just c

getLights :: Grid -> [Light]
getLights = concat
    . zipWith (map . Light) [1..]
    . groupOn headPos
    . sortBy (compare `on` headPos)
    . getRuns
    . zipGridWithCoords

-- get coordinates of first cell in a run (e.g. the start of "5 Across")
headPos :: Run -> Coord
headPos = head . coords

groupOn f = groupBy ((==) `on` f)

zipGridWithCoords :: Grid -> [[CellCoord]]
zipGridWithCoords grid = zipOverGrid grid coordsGrid

zipOverGrid = zipWith zip

coordsGrid = zipOverGrid (map repeat [0..]) (repeat [0..]) 

getRuns :: [[CellCoord]] -> [Run]
getRuns gwc = (getRuns' Across id) ++ (getRuns' Down transpose)
    where getRuns' dir f = concatMap (getRunsForLine dir) $ f gwc

getRunsForLine :: Dir -> [CellCoord] -> [Run]
getRunsForLine dir =
    map makeRun
    . filter isRun
    . groupOn isWhite
    where
        makeRun = Run dir . map snd
        isRun ((White _,_) : (White _,_) : _) = True
        isRun _ = False
        isWhite (Black, _) = False
        isWhite _ = True

getCoordLightMap :: [Light] -> CoordLightMap
getCoordLightMap ls = M.fromListWith (++) lightKVs
    where
        lightKVs = concatMap makeKV ls
        makeKV l = 
            let k = coords . run $ l
                v = repeat [l]
            in zip k v

getLightsForAnswer :: Grid -> [Light] -> Answer -> [Light]
getLightsForAnswer g ls a = undefined

matches :: (a -> String) -> [a] -> String -> [[a]]
matches _ _ [] = return []
matches f ss target = do
    s <- ss
    remainder <- maybeToList $ stripPrefix (f s) target
    fmap (s:) $ matches f ss remainder

longestMatch = longestMatchBy id

longestMatchBy :: (a -> String) -> [a] -> String -> Maybe [a]
longestMatchBy f ss target =
    let longestFirst = flip (compare `on` (length . f))
        sorted = sortBy longestFirst ss
    in listToMaybe $ matches f sorted target

class StringOnGrid a where
    stringOnGrid :: Grid -> a -> String

instance StringOnGrid Run where
    stringOnGrid g r =
        let cs = coords r
            indexIntoGrid (row,col) = stringify $ g !! row !! col
        in concatMap indexIntoGrid cs

instance StringOnGrid Light where
    stringOnGrid g l = stringOnGrid g (run l)

-- stringifications, for human-readable debugging
instance Stringify Cell where
    stringify Black = "#"
    stringify (White Nothing) = " "
    stringify (White (Just c)) = [c]

instance Stringify Crow where
    stringify c = 
        let gwc = zipGridWithCoords $ grid c
        in intercalate "\n" $ mapOverGrid charify gwc
        where
            charify (Black, _) = '#'
            charify (_, coord) = 
                let lights = getLightsForCoord c coord
                    charify' [_,_] = '+'
                    charify' [Light _ (Run Across _)] = '-'
                    charify' [Light _ (Run Down _)] = '|'
                in charify' lights

getLightsForCoord :: Crow -> Coord -> [Light]
getLightsForCoord = flip (M.findWithDefault []) . coordLightMap

instance Stringify Light where
    stringify l = intercalate " " [ lnum', dir', concat ["(", length', ")"] ]
        where
            r = run l
            lnum' = show . lnum $ l
            dir' = show . dir $ r
            length' = show . length . coords $ r
