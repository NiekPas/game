module Main where
import qualified Brick as B
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Brick (continueWithoutRedraw)
import Data.Either (fromRight)
import Control.Monad.State.Lazy (get, put, MonadIO (liftIO), StateT, evalStateT)


-- DATA TYPES

data Square = Empty | VWall | HWall | Player | Door Board
  deriving (Eq)

instance Show Square where
  show Empty = " "
  show VWall = "│"
  show HWall = "─"
  show Player = "P"
  show (Door _) = "▣"

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

-- TODO file not found handling
readMapFile :: FilePath -> IO Board
readMapFile s = readFile s >>= readMap

readMap :: String -> IO Board
readMap =
  parseLines . preprocess
  where
    preprocess = splitAtPredicate (== "---") . map (dropWhile (`elem` "1234567890: ")) . filter (/= "") . lines

parseLines :: ([String], [String]) -> IO Board
parseLines (lns, doorPaths) = evalStateT (go lns) 0
  where
    -- Determine the width of the board by looking at the longest line
    boardWidth :: [String] -> Int
    boardWidth rows = maximum (map length rows)
    go :: [String] -> StateT Int IO Board
    go [] = return V.empty
    go board@(row : rows) = do
      theseSquares <- get >>= \doorIndex -> mapM (readRoomChar doorIndex doorPaths) row
      let theseSquares' = (V.singleton . padRow (boardWidth board) . V.fromList) theseSquares
      nextSquares <- go rows
      return $ (V.++) theseSquares' nextSquares

padRow :: Int -> V.Vector Square  -> V.Vector Square
padRow width row = row V.++ V.replicate (width - V.length row) Empty

readRoomChar :: Int -> [FilePath] -> Char -> StateT Int IO Square
readRoomChar doorIndex doors c = do
  case c of
    '|' -> return VWall
    ' ' -> return Empty
    '_' -> return HWall
    'P' -> return Player
    '▣' -> do
      -- TODO safe indexing
      doorRoom <- liftIO $ readMapFile (doors !! doorIndex)
      put (doorIndex + 1)
      return (Door doorRoom)
    _ -> error "Map parse error"

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

-- | 'splitAtPredicate', applied to a predicate @p@ and a list @xs@,
-- splits the list at element @x@ satisfying @p@, dropping @x@.
-- If 'p' is not statisfied, the full list and the null list are returned as a tuple.
splitAtPredicate :: (a -> Bool) -> [a] -> ([a], [a])
splitAtPredicate _ [] = ([], [])
splitAtPredicate p xs = splitAtPredicateAcc p [] xs
  where
    splitAtPredicateAcc _ left [] = (left, [])
    splitAtPredicateAcc p' left (x : xs')
      | p' x = (left, xs')
      | otherwise = splitAtPredicateAcc p' (left ++ [x]) xs'
