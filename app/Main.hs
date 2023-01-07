module Main where
import qualified Brick as B
import qualified Data.Vector as V
import Data.Foldable (find)

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

boardSize :: Int
boardSize = 4

main :: IO ()
main = B.simpleMain ui

ui :: B.Widget ()
ui = renderBoard board

-- GENERATION

board :: Board
board = V.fromList rows

rows :: [V.Vector Square]
rows = map generateRow [0..(boardSize - 1)]

generateRow :: Int -> V.Vector Square
generateRow y = V.generate boardSize (\x -> generateSquare (x, y))

generateSquare :: Coordinate -> Square
generateSquare c = maybe Empty snd (find isCoordinate squares)
  where
    isCoordinate (c', _) = c' == c

-- `squares` maps coordinates to squares
squares :: [((Int, Int), Square)]
squares =
  [ ((0, 0), X)
  , ((1, 0), X)
  , ((2, 0), Empty)
  , ((3, 0), X)
  , ((0, 1), X)
  , ((1, 1), X)
  , ((2, 1), Empty)
  , ((3, 1), X)
  , ((0, 2), X)
  , ((1, 2), X)
  , ((2, 2), Empty)
  , ((3, 2), X)
  , ((0, 3), X)
  , ((1, 3), X)
  , ((2, 3), Player)
  , ((3, 3), X)
  ]

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
