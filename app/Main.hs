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
  let c = (2, 0)
  let newBoard = setPlayerLocation c board
  B.simpleMain $ renderBoard newBoard

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

-- PLAYER LOCATION

getPlayerLocation :: Board -> Maybe Coordinate
getPlayerLocation board = do
  y <- getPlayerRow
  x <- getPlayerColumn y
  Just (x, y)
  where
    getPlayerRow = V.findIndex (`contains` Player) board
    getPlayerColumn :: Int -> Maybe Int
    getPlayerColumn y' =
      let row = board V.! y' in V.elemIndex Player row

setPlayerLocation :: Coordinate -> Board -> Board
setPlayerLocation c = setPlayer c . removePlayer
  where
    removePlayer :: Board -> Board
    removePlayer = V.map removePlayerRow
      where
        removePlayerRow :: V.Vector Square -> V.Vector Square
        removePlayerRow = V.map removePlayerSquare
        removePlayerSquare :: Square -> Square
        removePlayerSquare Player = Empty
        removePlayerSquare x = x
    setPlayer :: Coordinate -> Board -> Board
    setPlayer c board = board V.// [(y, row V.// [(x, Player)])]
      where
        (x, y) = c
        row = board V.! y

-- UTIL

-- `contains` is a flipped version of `elem` which is arguably more readable
contains :: Eq a => V.Vector a -> a -> Bool
contains = flip V.elem
