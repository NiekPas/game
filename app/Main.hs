module Main where
import qualified Brick as B
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Brick (continueWithoutRedraw)
import Data.Either (fromRight)
import Data.List (find)

-- DATA TYPES

data Square = Empty | VWall | HWall | Player | Door Board
  deriving (Eq)

instance Show Square where
  show Empty = " "
  show VWall = "│"
  show HWall = "─"
  show Player = "P"
  show (Door _) = "▣"

instance Read Square where
  readsPrec _ " " = [(Empty, "")]
  readsPrec _ "|" = [(VWall, "")]
  readsPrec _ "_" = [(HWall, "")]
  readsPrec _ "P" = [(Player, "")]
  -- TODO dynamic links
  readsPrec _ "▣" = [(Door testBoard, "")]
  readsPrec _ _ = []

data MovementDirection = U | D | L | R

type Board = V.Vector (V.Vector Square)

type AppState = Board

type Coordinate = (Int, Int)

-- APP

gameApp :: B.App AppState e ()
gameApp = B.App
  { B.appDraw = renderBoard
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent = handleEvent
  , B.appStartEvent =  return ()
  , B.appAttrMap = const attrMap
}

handleEvent :: B.BrickEvent () e -> B.EventM () AppState ()
handleEvent (B.VtyEvent e) = handleVtyEvent e
handleEvent _            = B.continueWithoutRedraw

handleVtyEvent :: Vty.Event -> B.EventM n AppState ()
handleVtyEvent (Vty.EvKey Vty.KUp _modifiers) = B.modify $ wrapMoveCursor U
handleVtyEvent (Vty.EvKey Vty.KDown _modifiers) = B.modify $ wrapMoveCursor D
handleVtyEvent (Vty.EvKey Vty.KLeft _modifiers) = B.modify $ wrapMoveCursor L
handleVtyEvent (Vty.EvKey Vty.KRight _modifiers) = B.modify $ wrapMoveCursor R
handleVtyEvent (Vty.EvKey (Vty.KChar 'q') []) = B.halt
handleVtyEvent _ = continueWithoutRedraw

wrapMoveCursor :: MovementDirection -> AppState -> AppState
wrapMoveCursor dir board = fromRight board $ movePlayer dir board

attrMap :: B.AttrMap
attrMap = B.attrMap Vty.defAttr []

-- UI

main :: IO ()
main = do
  board <- readMapFile "map5.map"
  _finalState <- B.defaultMain gameApp board
  putStrLn "goodbye"

-- RENDERING

renderBoard :: Board -> [B.Widget ()]
renderBoard = (: []) . B.vBox . V.toList . V.map renderRow

renderRow :: V.Vector Square -> B.Widget ()
renderRow = B.str . concat . V.toList . V.map show

-- READING MAP FILES

readMapFile :: FilePath -> IO Board
readMapFile = fmap readMap . readFile

readMap :: String -> Board
readMap s =
  let
    rows :: [V.Vector Square]
    rows = map readRow (lines s)
    boardWidth = maximum (map V.length rows)
    padRow :: V.Vector Square -> V.Vector Square
    padRow row = row V.++ V.replicate (boardWidth - V.length row) Empty
  in V.fromList (map padRow rows)

readRow :: String -> V.Vector Square
readRow = V.fromList . map readSquare
  where
    readSquare c = read [c] :: Square

-- MOVEMENT

movePlayer :: MovementDirection -> Board -> Either () Board
movePlayer dir board = case getPlayerLocation board of
  Nothing -> Left ()
  Just (x, y) -> case dir of
    U -> setPlayerLocation (x, y - 1) board
    D -> setPlayerLocation (x, y + 1) board
    L -> setPlayerLocation (x - 1, y) board
    R -> setPlayerLocation (x + 1, y) board

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

-- |`setPlayerLocation` attempts to set the player location to the given coordinate.
-- If the given coordinate is occupied, it returns `Left ()`.
setPlayerLocation :: Coordinate -> Board -> Either () Board
setPlayerLocation c board = maybe (Left ()) goToSquare (getSquare c board)
  where
    goToSquare :: Square -> Either () Board
    goToSquare Empty = (Right . setPlayer c . removePlayer) board
    goToSquare (Door nextBoard) = Right nextBoard
    goToSquare _ = Left ()
    removePlayer :: Board -> Board
    removePlayer = V.map removePlayerRow
      where
        removePlayerRow :: V.Vector Square -> V.Vector Square
        removePlayerRow = V.map removePlayerSquare
        removePlayerSquare :: Square -> Square
        removePlayerSquare Player = Empty
        removePlayerSquare x = x
    setPlayer :: Coordinate -> Board -> Board
    setPlayer c' board' = board' V.// [(y, row V.// [(x, Player)])]
      where
        (x, y) = c'
        row = board' V.! y

-- UTIL

-- `contains` is a flipped version of `elem` which is arguably more readable
contains :: Eq a => V.Vector a -> a -> Bool
contains = flip V.elem

getSquare :: Coordinate -> Board -> Maybe Square
getSquare = getSquare'
  where
    getSquare' :: Coordinate -> Board -> Maybe Square
    getSquare' (x, y) board = do
      row <- board V.!? y
      row V.!? x


testBoard :: Board
testBoard = V.fromList testRows

testRows :: [V.Vector Square]
testRows = map generateRow [0..(boardSize - 1)]

generateRow :: Int -> V.Vector Square
generateRow y = V.generate boardSize (\x -> generateSquare (x, y))

boardSize :: Int
boardSize = 2

generateSquare :: Coordinate -> Square
generateSquare c = maybe Empty snd (find isCoordinate squares)
  where
    isCoordinate (c', _) = c' == c

-- `squares` maps coordinates to squares
squares :: [((Int, Int), Square)]
squares =
  [ ((0, 0), VWall)
  , ((1, 0), VWall)
  , ((0, 1), Empty)
  , ((1, 1), Player)
  ]
