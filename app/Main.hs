module Main where
import qualified Brick as B
import qualified Data.Vector as V

-- DATA TYPES

data Square = Empty | X | Player
  deriving (Eq)

instance Show Square where
  show Empty = " "
  show X = "|"
  show Player = "P"

instance Read Square where
  readsPrec _ " " = [(Empty, "")]
  readsPrec _ "|" = [(X, "")]
  readsPrec _ "P" = [(Player, "")]
  readsPrec _ _ = []

type Board = V.Vector (V.Vector Square)

type Coordinate = (Int, Int)

-- UI

main :: IO ()
main = do
  board <- readMapFile "map1.gmap"
  B.simpleMain $ renderBoard board

-- RENDERING

renderBoard :: Board -> B.Widget ()
renderBoard = B.vBox . V.toList . V.map renderRow

renderRow :: V.Vector Square -> B.Widget ()
renderRow = B.str . concat . V.toList . V.map show

-- READING MAP FILES

readMapFile :: FilePath -> IO Board
readMapFile = fmap readMap . readFile

readMap :: String -> Board
readMap = V.fromList . map readRow . lines

readRow :: String -> V.Vector Square
readRow = V.fromList . map readSquare
  where
    readSquare c = read [c] :: Square
