module Crow
    where

import Data.List
import Data.Function
import qualified Data.Map as M

-- Example:  putStrLn . stringify . parseGrid2Crow $ cw  -- (to work on sample grid in Spec.hs)

-- Direction
data Dir = Across | Down
    deriving Show

data Cell = Black | White (Maybe Char)
    deriving (Show, Eq)

type Grid = [[Cell]]

type Coord = (Int,Int)

-- a Run of White cells, associated with a direction
data Run = Run
    { dir :: Dir
    , coords :: [Coord]
    }
    deriving Show

-- a numbered run (1-based), e.g. "5 Across" or "2 Down"
data Light = Light
    { lnum :: Int
    , run :: Run
    }
    deriving Show

-- get list of lights that a given coordinate is in
type CoordLightMap = M.Map Coord [Light]

data Crow = Crow
    { grid :: Grid
    , lights :: [Light]
    , coordLightMap :: CoordLightMap
    }
    deriving Show

type Enumeration = [Int]

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
parseGrid lines = mapOverGrid parseCell lines

mapOverGrid = map.map

parseCell :: Char -> Cell
parseCell '#' = Black
parseCell ' ' = White Nothing
parseCell c = White $ Just c

getLights :: Grid -> [Light]
getLights grid =
    let gwc = getGridWithCoords grid
        acrosses = getRuns Across gwc
        downs = getRuns Down $ transpose gwc
        all = sortBy (compare `on` headPos) $ acrosses ++ downs
        grouped = groupBy ((==) `on` headPos) all
        makeLightNs g n = map (Light n) g
        numbered = zipWith makeLightNs grouped [1..]
    in concat numbered

-- get coordinates of first cell in a run (e.g. the start of "5 Across")
headPos :: Run -> Coord
headPos = head . coords

getGridWithCoords :: Grid -> [[ (Cell, Coord) ]]
getGridWithCoords grid = zipOverGrid grid coordsGrid

zipOverGrid = zipWith zip

coordsGrid = zipOverGrid (map repeat [0..]) (repeat [0..]) 

getRuns :: Dir -> [[(Cell, Coord)]] -> [Run]
getRuns d = concatMap $ getRunsForLine d

-- parse a row/column of a Grid into Runs
getRunsForLine :: Dir -> [(Cell, Coord)] -> [Run]
getRunsForLine dir line =
    let groups :: [[(Cell, Coord)]]
        groups = groupBy ((==) `on` isWhite ) line

        isWhite :: (Cell, Coord) -> Bool
        isWhite (Black, _) = False
        isWhite _ = True

        isRun :: [(Cell, Coord)] -> Bool
        isRun ( (White _,_) : (White _,_) : _) = True
        isRun _ = False

        makeRun :: [ (Cell, Coord) ] -> Run
        makeRun cgs =
            let coords = map snd cgs
            in Run dir coords
    in map makeRun $ filter isRun groups

getCoordLightMap :: [Light] -> CoordLightMap
getCoordLightMap ls =
    let coord2l l = zip (coords . run $ l) $ repeat [l]
        ls' = concatMap coord2l ls
    in M.fromListWith (++) ls'

-- stringifications, for human-readable debugging
instance Stringify Cell where
    stringify Black = "#"
    stringify (White Nothing) = " "
    stringify (White (Just c)) = c : []

instance Stringify Crow where
    stringify c = 
        let gwc :: [[ (Cell, Coord) ]]
            gwc = getGridWithCoords $ grid c
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
getLightsForCoord c coord = M.findWithDefault [] coord $ coordLightMap c

instance Stringify Light where
    stringify l =
        let r = run l
        in intercalate " "
            [ (show . lnum $ l)
            , (show . dir $ r)
            , "(" ++ (show . length . coords $ r) ++ ")"
            ]
