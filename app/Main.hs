{-# LANGUAGE LambdaCase #-}
module Main where
import qualified Brick as B
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Brick (continueWithoutRedraw)
import Control.Monad.State.Lazy (get, put, MonadIO (liftIO), StateT, evalStateT)
import Control.Exception (IOException, catch, Exception)
import Data.Functor ((<&>))
import Control.Monad.Catch (throwM)

-- DATA TYPES

data Square = Empty | VWall | HWall | Player | Door Room | Pickup Item
  deriving (Eq)

-- Possible results from trying to move the player to a new coordinate.
data MoveResult = Success Room (Maybe MoveSideEffect) | Failure

newtype MoveSideEffect = ItemPickedUp Item

instance Show Square where
  show Empty = " "
  show VWall = "│"
  show HWall = "─"
  show Player = "P"
  show (Door _) = "▣"
  show (Pickup Sword) = "🗡️"

data MovementDirection = U | D | L | R

data Item = Sword
  deriving (Eq)

instance Show Item where
  show Sword = "🗡️"

data AppState = AppState { room :: Room, inventory :: [Item] }

type Room = V.Vector (V.Vector Square)

type Coordinate = (Int, Int)

data RoomParseException = RoomParseException
  deriving (Show)
instance Exception RoomParseException

-- APP

gameApp :: B.App AppState e ()
gameApp = B.App
  { B.appDraw = ui
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

-- TODO rename
wrapMoveCursor :: MovementDirection -> AppState -> AppState
wrapMoveCursor dir appState = case movePlayerInDirection dir (room appState) of
  Success nextRoom Nothing -> appState { room = nextRoom }
  Success nextRoom (Just (ItemPickedUp item)) -> appState { room = nextRoom, inventory = item : inventory appState}
  Failure -> appState

attrMap :: B.AttrMap
attrMap = B.attrMap Vty.defAttr []

-- UI

main :: IO ()
main = do
  room' <- readMapFile "map7.map"
  _ <- case room' of
    Nothing -> error "Can't find the map bye"
    Just r -> B.defaultMain gameApp AppState {room = r, inventory = []}
  putStrLn "goodbye"

ui :: AppState -> [B.Widget ()]
ui appState = [renderRoom (room appState), renderInventory (inventory appState)]

-- RENDERING

renderRoom :: Room -> B.Widget ()
renderRoom = B.vBox . (: []) . B.vBox . V.toList . V.map renderRow

renderRow :: V.Vector Square -> B.Widget ()
renderRow = B.str . concat . V.toList . V.map show

renderInventory :: [Item] -> B.Widget ()
renderInventory = B.padTop B.Max . B.hBox . map (B.str . show)

-- READING MAP FILES

readMapFile :: FilePath -> IO (Maybe Room)
readMapFile s = safeReadFile s >>= \case
  Nothing -> return Nothing
  Just str -> (readMap str <&> Just)
  where
    safeReadFile :: FilePath -> IO (Maybe String)
    safeReadFile path =
        (Just <$> readFile path) `catch`
        ((\_ -> pure Nothing) :: IOException -> IO (Maybe String))

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
    '🗡' -> return $ Pickup Sword
    '▣' -> do
      -- TODO safe indexing
      doorRoom <- liftIO $ readMapFile (doors !! doorIndex)
      put (doorIndex + 1)
      case doorRoom of
        Just d -> return (Door d)
        Nothing -> throwM RoomParseException
    _ -> throwM RoomParseException

-- MOVEMENT

movePlayerInDirection :: MovementDirection -> Room -> MoveResult
movePlayerInDirection dir room' = case getPlayerLocation room' of
  Nothing -> Failure
  Just (x, y) -> setPlayerLocation (x', y') room'
    where
      (x', y') = case dir of
        U -> (x, y - 1)
        D -> (x, y + 1)
        L -> (x - 1, y)
        R -> (x + 1, y)

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
-- If the given coordinate has a pickup, the item is added to the inventory.
setPlayerLocation :: Coordinate -> Room -> MoveResult
setPlayerLocation c room' = maybe Failure goToSquare (getSquare c room')
  where
    goToSquare :: Square -> MoveResult
    goToSquare Empty            = let nextRoom = (setPlayer c . removePlayer) room' in Success nextRoom Nothing
    goToSquare (Pickup item)    = let nextRoom = (setPlayer c . removePlayer) room' in Success nextRoom $ Just (ItemPickedUp item)
    goToSquare (Door nextRoom)  = Success nextRoom Nothing
    goToSquare _                = Failure

    removePlayer :: Room -> Room
    removePlayer = V.map removePlayerRow
      where
        removePlayerRow :: V.Vector Square -> V.Vector Square
        removePlayerRow = V.map removePlayerSquare
        removePlayerSquare :: Square -> Square
        removePlayerSquare Player = Empty
        removePlayerSquare x      = x

    setPlayer :: Coordinate -> Room -> Room
    setPlayer c' room'' = room'' V.// [(y, row V.// [(x, Player)])]
      where
        (x, y)  = c'
        row     = room'' V.! y

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
