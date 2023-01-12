module Main where
import qualified Brick as B
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Brick (continueWithoutRedraw)
import Data.Either (fromRight)
import Control.Monad.State.Lazy (get, put, MonadIO (liftIO), StateT, evalStateT)


-- DATA TYPES

data Square = Empty | VWall | HWall | Player | Door Room
  deriving (Eq)

instance Show Square where
  show Empty = " "
  show VWall = "│"
  show HWall = "─"
  show Player = "P"
  show (Door _) = "▣"

data MovementDirection = U | D | L | R

data Item = Sword

data AppState = AppState { room :: Room, inventory :: [Item] }

type Room = V.Vector (V.Vector Square)

type Coordinate = (Int, Int)

-- APP

gameApp :: B.App AppState e ()
gameApp = B.App
  { B.appDraw = renderBoard . room
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
wrapMoveCursor dir appState = appState { room = fromRight (room appState) $ movePlayer dir (room appState) }

attrMap :: B.AttrMap
attrMap = B.attrMap Vty.defAttr []

-- UI

main :: IO ()
main = do
  room' <- readMapFile "map5.map"
  _finalState <- B.defaultMain gameApp AppState {room = room', inventory = []}
  putStrLn "goodbye"

-- RENDERING

renderBoard :: Room -> [B.Widget ()]
renderBoard = (: []) . B.vBox . V.toList . V.map renderRow

renderRow :: V.Vector Square -> B.Widget ()
renderRow = B.str . concat . V.toList . V.map show

-- READING MAP FILES

-- TODO file not found handling
readMapFile :: FilePath -> IO Room
readMapFile s = readFile s >>= readMap

readMap :: String -> IO Room
readMap =
  parseLines . preprocess
  where
    preprocess = splitAtPredicate (== "---") . map (dropWhile (`elem` "1234567890: ")) . filter (/= "") . lines

parseLines :: ([String], [String]) -> IO Room
parseLines (lns, doorPaths) = evalStateT (go lns) 0
  where
    -- Determine the width of the room by looking at the longest line
    boardWidth :: [String] -> Int
    boardWidth rows = maximum (map length rows)
    go :: [String] -> StateT Int IO Room
    go [] = return V.empty
    go room'@(row : rows) = do
      theseSquares <- get >>= \doorIndex -> mapM (readRoomChar doorIndex doorPaths) row
      let theseSquares' = (V.singleton . padRow (boardWidth room') . V.fromList) theseSquares
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

movePlayer :: MovementDirection -> Room -> Either () Room
movePlayer dir room' = case getPlayerLocation room' of
  Nothing -> Left ()
  Just (x, y) -> case dir of
    U -> setPlayerLocation (x, y - 1) room'
    D -> setPlayerLocation (x, y + 1) room'
    L -> setPlayerLocation (x - 1, y) room'
    R -> setPlayerLocation (x + 1, y) room'

-- PLAYER LOCATION

getPlayerLocation :: Room -> Maybe Coordinate
getPlayerLocation room' = do
  y <- getPlayerRow
  x <- getPlayerColumn y
  Just (x, y)
  where
    getPlayerRow = V.findIndex (`contains` Player) room'
    getPlayerColumn :: Int -> Maybe Int
    getPlayerColumn y' =
      let row = room' V.! y' in V.elemIndex Player row

-- |`setPlayerLocation` attempts to set the player location to the given coordinate.
-- If the given coordinate is occupied, it returns `Left ()`.
setPlayerLocation :: Coordinate -> Room -> Either () Room
setPlayerLocation c room' = maybe (Left ()) goToSquare (getSquare c room')
  where
    goToSquare :: Square -> Either () Room
    goToSquare Empty = (Right . setPlayer c . removePlayer) room'
    goToSquare (Door nextBoard) = Right nextBoard
    goToSquare _ = Left ()
    removePlayer :: Room -> Room
    removePlayer = V.map removePlayerRow
      where
        removePlayerRow :: V.Vector Square -> V.Vector Square
        removePlayerRow = V.map removePlayerSquare
        removePlayerSquare :: Square -> Square
        removePlayerSquare Player = Empty
        removePlayerSquare x = x
    setPlayer :: Coordinate -> Room -> Room
    setPlayer c' room'' = room'' V.// [(y, row V.// [(x, Player)])]
      where
        (x, y) = c'
        row = room'' V.! y

-- UTIL

-- `contains` is a flipped version of `elem` which is arguably more readable
contains :: Eq a => V.Vector a -> a -> Bool
contains = flip V.elem

getSquare :: Coordinate -> Room -> Maybe Square
getSquare = getSquare'
  where
    getSquare' :: Coordinate -> Room -> Maybe Square
    getSquare' (x, y) room' = do
      row <- room' V.!? y
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
