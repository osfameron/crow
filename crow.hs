import Data.List
import Data.Function

class (Show a) => Stringify a where
    stringify :: a -> String
    stringify = show

data Dir = Across | Down
    deriving Show
data Cell = Black | White (Maybe Char)
    deriving (Show, Eq)

data GridCell = GridCell Coord Cell
    deriving Show

data Run = Run Dir [GridCell]
    deriving Show

data Light = Light Int Run
    deriving Show

instance Stringify Light where
    stringify (Light i (Run d cs)) = intercalate " " [(show i), (show d), "(" ++ (show . length $ cs) ++ ")"]

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

parseGrid :: [ String ] -> [ [ GridCell ] ]
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

getAllLights grid =
    let acrosses = concatMap (getRuns Across) $ parseGrid cw
        downs = concatMap (getRuns Down) . transpose $ parseGrid cw
        all = sortBy (compare `on` headPos) $ acrosses ++ downs
        grouped = groupBy ((==) `on` headPos) all
        makeLightNs g n = map (Light n) g
        numbered = zipWith makeLightNs grouped [1..]
    in concat numbered

