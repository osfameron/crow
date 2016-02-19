import Data.List
import Data.Function
import qualified Data.Map as M

-- Direction
data Dir = Across | Down
    deriving Show

data Cell = Black | White (Maybe Char)
    deriving (Show, Eq)

type Coord = (Int,Int)

data GridCell = GridCell Coord Cell
    deriving Show

type Grid = [[GridCell]]

-- a Run of White cells, associated with a direction
data Run = Run Dir [GridCell]
    deriving Show

-- a numbered run (1-based), e.g. "5 Across" or "2 Down"
data Light = Light Int Run
    deriving Show

-- get list of lights that a given coordinate is in
type CoordLightMap = M.Map Coord [Light]

-- the whole grid
data Crow = Crow Grid CoordLightMap
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

-- call on the sample grid as above
parseGrid2Crow :: [String] -> Crow
parseGrid2Crow lines =
    let grid = parseGrid lines
        lights = getLights grid
        lm = getCoordLightMap lights
    in Crow grid lm

parseGrid :: [String] -> Grid
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

getLights :: Grid -> [Light]
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
headPos (Run _ ( (GridCell pos _) : _ )) = pos

-- parse a row/column of a Grid into Runs
getRuns :: Dir -> [GridCell] -> [Run]
getRuns dir line =
    let groups = groupBy ((==) `on` isWhite ) line
        isRun ((GridCell _ (White _)) : (GridCell _ (White _)) : _) = True
        isRun _ = False
    in map (Run dir) . (filter isRun) $ groups

isWhite :: GridCell -> Bool
isWhite (GridCell _ Black) = False
isWhite _ = True

getCoordLightMap :: [Light] -> CoordLightMap
getCoordLightMap ls =
    let coord2l l@(Light _ (Run _ gcs)) =
            let coords = map (\(GridCell pos _) -> pos) gcs
            in zip coords $ repeat [l]
        ls' = concatMap coord2l ls
    in M.fromListWith (++) ls'

-- stringifications, for human-readable debugging
instance Stringify Crow where
    stringify (Crow grid lm) = "TODO"

instance Stringify Light where
    stringify (Light i (Run d cs)) = intercalate " " [(show i), (show d), "(" ++ (show . length $ cs) ++ ")"]

instance Stringify Clue where
    stringify (Clue s l e) = 
        let l' = intercalate ", " $ map (\(Light i (Run d _)) -> (show i) ++ " " ++ (show d)) l
            e' = "(" ++ (intercalate "," $ map show e) ++ ")"
        in intercalate " " [l' ++ ".", s, e']
