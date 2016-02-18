import Data.List
import Data.Function
import qualified Data.Map as M

class (Show a) => Stringify a where
    stringify :: a -> String
    stringify = show

data Dir = Across | Down
    deriving Show
data Cell = Black | White (Maybe Char)
    deriving (Show, Eq)

data GridCell = GridCell Coord Cell
    deriving Show

type Grid = [[GridCell]]

data Run = Run Dir [GridCell]
    deriving Show

data Light = Light Int Run
    deriving Show

data Crow = Crow Grid (M.Map Coord [Light])
    deriving Show

instance Stringify Light where
    stringify (Light i (Run d cs)) = intercalate " " [(show i), (show d), "(" ++ (show . length $ cs) ++ ")"]

data Clue = Clue String [Light] [Int]
    deriving Show

instance Stringify Clue where
    stringify (Clue s l e) = 
        let l' = intercalate ", " $ map (\(Light i (Run d _)) -> (show i) ++ " " ++ (show d)) l
            e' = "(" ++ (intercalate "," $ map show e) ++ ")"
        in intercalate " " [l' ++ ".", s, e']

type Coord = (Int,Int)
type Length = Int 

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

parseCell '#' = Black
parseCell ' ' = White Nothing
parseCell c = White $ Just c

isWhite (GridCell _ Black) = False
isWhite _ = True

parseGrid :: [ String ] -> Grid
parseGrid lines =
    let rows = zipWith makeRow [0..] $ lines
        makeRow rownum row = 
            let makeCell colnum char = GridCell (rownum, colnum) $ parseCell char
            in zipWith makeCell [0..] row
    in rows

getRuns dir line =
    let groups = groupBy ((==) `on` isWhite ) line
        isRun ((GridCell _ (White _)) : (GridCell _ (White _)) : _) = True
        isRun _ = False
    in map (Run dir) . (filter isRun) $ groups

headPos (Run _ ( (GridCell pos _) : _ )) = pos

getLights :: Grid -> [Light]
getLights grid =
    let acrosses = concatMap (getRuns Across) $ parseGrid cw
        downs = concatMap (getRuns Down) . transpose $ parseGrid cw
        all = sortBy (compare `on` headPos) $ acrosses ++ downs
        grouped = groupBy ((==) `on` headPos) all
        makeLightNs g n = map (Light n) g
        numbered = zipWith makeLightNs grouped [1..]
    in concat numbered

getGridCell2LightMap ls =
    let coord2l l@(Light _ (Run _ gcs)) =
            let coords = map (\(GridCell pos _) -> pos) gcs
            in zip coords $ repeat [l]
        ls' = concatMap coord2l ls
    in M.fromListWith (++) ls'

parseGrid2Crow lines =
    let grid = parseGrid lines
        lights = getLights grid
        lm = getGridCell2LightMap lights
    in Crow grid lm

instance Stringify Crow where
    stringify (Crow grid lm) = "TODO"

