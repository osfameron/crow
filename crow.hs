import Data.List
import Data.Function
import qualified Data.Map as M

-- Direction
data Dir = Across | Down
    deriving Show

data Cell = Black | White (Maybe Char)
    deriving (Show, Eq)

type Coord = (Int,Int)

data GridCell = GridCell
    { coord :: Coord
    , cell  :: Cell
    }
    deriving Show

-- a Run of White cells, associated with a direction
data Run = Run
    { dir :: Dir
    , gcs :: [GridCell]
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

data CrowCell = CrowCell
    { gc :: GridCell
    , lights :: [Light]
    }
    deriving Show

-- the whole grid
data Crow = Crow [[CrowCell]]
    deriving Show

type Enumeration = [Int]

-- e.g. Clue "So cryptic!" [light_5_across, light_2_down] [5, 10]
data Clue = Clue String [Light] Enumeration
    deriving Show

-- Haskell's standard Show is great for machine-readable output, but we also want
-- human-readable for debugging.  This typeclass lets us define a 'stringify' function
class (Show a) => Stringify a where
    stringify :: a -> String
    stringify = show

cw = [
  "TRIPOD#        ",
  "# # # # # # # #",
  "PARSIFAL#RESCUE",
  "# # # # # # # #",
  "###            ",
  "# # # # # ### #",
  "ANNA###        ",
  "# # # # # # # #",
  "        ###PARE",
  "# ### # #U# # #",
  "         S  ###",
  "# # # # #E# # #",
  "      #INFERNAL",
  "# # # # #U# # #",
  "FRAGMENT#L     "]

-- e.g. parseGrid2Crow cw  (to work on sample grid above)
parseGrid2Crow :: [String] -> Crow
parseGrid2Crow lines =
    let grid = parseGrid lines
        lights = getLights grid
        lm = getCoordLightMap lights
        makeCC gc = CrowCell gc $ M.findWithDefault [] (coord gc) lm
    in Crow $ (map.map) makeCC grid

parseGrid :: [String] -> [[GridCell]]
parseGrid lines =
    let rows = zipWith makeRow [0..] $ lines
        makeRow rownum row = 
            let makeCell colnum char = GridCell (rownum, colnum) $ parseCell char
            in zipWith makeCell [0..] row
    in rows

parseCell :: Char -> Cell
parseCell '#' = Black
parseCell ' ' = White Nothing
parseCell c = White $ Just c

getLights :: [[GridCell]] -> [Light]
getLights grid =
    let acrosses = concatMap (getRuns Across) grid
        downs = concatMap (getRuns Down) $ transpose grid
        all = sortBy (compare `on` headPos) $ acrosses ++ downs
        grouped = groupBy ((==) `on` headPos) all
        makeLightNs g n = map (Light n) g
        numbered = zipWith makeLightNs grouped [1..]
    in concat numbered

-- get coordinates of first cell in a run (e.g. the start of "5 Across")
headPos :: Run -> Coord
headPos = coord . head . gcs

-- parse a row/column of a [[GridCell]] into Runs
getRuns :: Dir -> [GridCell] -> [Run]
getRuns dir line =
    let groups = groupBy ((==) `on` isWhite ) line
        isRun (GridCell {cell=White _} : GridCell {cell=White _} : _) = True
        isRun _ = False
    in map (Run dir) . filter isRun $ groups

isWhite :: GridCell -> Bool
isWhite GridCell { cell=Black } = False
isWhite _ = True

getCoordLightMap :: [Light] -> CoordLightMap
getCoordLightMap ls =
    let coord2l l =
            let coords = map coord $ gcs . run $ l
            in zip coords $ repeat [l]
        ls' = concatMap coord2l ls
    in M.fromListWith (++) ls'

-- stringifications, for human-readable debugging
instance Stringify Crow where
    stringify _ = "TODO"

instance Stringify Light where
    stringify l =
        let r = run l
        in intercalate " " [(show . lnum $ l), (show . dir $ r), "(" ++ (show . length . gcs $ r) ++ ")"]

instance Stringify Clue where
    stringify (Clue s ls e) = 
        let ls' = intercalate ", " $ map (\l -> (show . lnum $ l) ++ " " ++ (show . dir . run $ l)) ls
            e' = "(" ++ (intercalate "," $ map show e) ++ ")"
        in intercalate " " [ls' ++ ".", s, e']
