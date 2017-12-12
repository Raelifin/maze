{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( start
    ) where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Control.Monad.Loops        as Loop
import           Data.Array.IArray          ((!))
import qualified Data.Array.IArray          as IArr
import qualified Data.Array.IO              as IOArr
import qualified Data.Array.MArray          as MArr
import           Fmt                        (( #| ), (|#))
import qualified Graphics.Vty               as V
import           System.IO.Unsafe           (unsafePerformIO)
import qualified System.Random              as Rand

start :: IO ()
start = do
  gen <- Rand.getStdGen
  let mazeSize = 11
  maze <- genMaze mazeSize mazeSize gen
  finalState <- defaultMain mazeApp maze
  return ()

data XY = XY Int Int deriving (Eq, Ord, MArr.Ix)
instance Show XY where
  show (XY x y) = "(" #| x |# "," #| y |# ")"

asTuple :: XY -> (Int, Int)
asTuple (XY x y) = (x, y)

plus :: XY -> XY -> XY
plus (XY x1 y1) (XY x2 y2) = XY (x1+x2) (y1+y2)

data Maze = Maze { board :: IArr.Array XY Space, dim :: XY, playerPos :: XY }
data Space = Floor | Wall deriving Eq

type MazeSeed = (IOArr.IOArray XY Space, XY, XY, Int, [XY])

genMaze :: Int -> Int -> Rand.StdGen -> IO Maze
genMaze width height gen = do
  let (genForOtherThings, dirGen) = Rand.split gen
  let randomDeltas = map intToDelta $ Rand.randomRs (0 :: Int, 3) dirGen
  let dim = XY width height
  let startingPos = XY 1 1
  floorplan <- IOArr.newArray (XY 0 0, dim) Wall :: IO (IOArr.IOArray XY Space)
  MArr.writeArray floorplan startingPos Floor
  let mazeSeed = (floorplan, dim, startingPos, 0, randomDeltas)
  Loop.iterateUntilM goodMaze buildMaze mazeSeed
  -- let validSquares = zip [1, 3 .. width] [1, 3 .. height]
  -- TODO: Find some notion of whether or not a valid square can exit the maze
  -- TODO: Fill in the dead-ends in the rest of the maze
  frozenFloorplan <- MArr.freeze floorplan
  return $ Maze frozenFloorplan dim startingPos

goodMaze :: MazeSeed -> Bool
goodMaze (_, dim@(XY x y), pos, pathLength, _) = pathLength > x + y && canExitInOneStep dim pos

pathLengthThreshold :: Int
pathLengthThreshold = 21

canExitInOneStep :: XY -> XY -> Bool
canExitInOneStep (XY width height) (XY x y) = x < 2 || y < 2 || x > width - 3 || y > height - 3

buildMaze :: MazeSeed-> IO MazeSeed
buildMaze (floorplan, dim, pos, pathLength, nextDelta : deltas)
  | freshGround nextDelta maze = advance nextDelta maze
  | canAdvance nextDelta maze && unsafeIsWall nextDelta maze
    = either (`advance` maze) (`backOut` maze) (searchForAlternativeRoutes nextDelta maze)
  | otherwise = return maze  -- We tried to make an invalid move. Try again, ignoring this delta.
  where maze = (floorplan, dim, pos, pathLength, deltas)

-- | Uses unsafePerformIO to check whether a relative position on our floorplan is a Wall
unsafeIsWall :: XY -> MazeSeed -> Bool
unsafeIsWall delta (floorplan, _, pos, _, _)
  = unsafePerformIO (MArr.readArray floorplan (pos `plus` delta)) == Wall

-- | Checks whether we're about to fall off the map
canAdvance :: XY -> MazeSeed -> Bool
canAdvance nextDelta (_, dim, pos, _, _) = nextPos `isInsideDim` dim where
  nextPos = pos `plus` nextDelta `plus` nextDelta

-- | Warning: Unsafe. Use with caution.
advance :: XY -> MazeSeed -> IO MazeSeed
advance nextDelta (floorplan, dim, pos, pathLength, deltas) = do
  nextPos <- pure $ pos `plus` nextDelta `plus` nextDelta
  MArr.writeArray floorplan nextPos Floor
  MArr.writeArray floorplan (pos `plus` nextDelta) Floor
  return (floorplan, dim, nextPos, pathLength + 2, deltas)

-- | Warning: Unsafe. Use with caution.
backOut :: XY -> MazeSeed -> IO MazeSeed
backOut wayBack (floorplan, dim, pos, pathLength, deltas) = do
  -- Build a wall behind us to keep it unambiguous where we've been.
  MArr.writeArray floorplan (pos `plus` wayBack) Wall
  -- Return to previous position
  return (floorplan, dim, pos `plus` wayBack `plus` wayBack, pathLength - 2, deltas)

searchForAlternativeRoutes :: XY -> MazeSeed -> Either XY XY
searchForAlternativeRoutes firstAttempt maze
  | freshGround (rotateCW firstAttempt) maze   = Left (rotateCW firstAttempt)
  | freshGround (rotateCCW firstAttempt) maze  = Left (rotateCCW firstAttempt)
  | freshGround (invert firstAttempt) maze     = Left (invert firstAttempt)
  | otherwise                                  = Right (findWayBack firstAttempt maze)

rotateCW :: XY -> XY
rotateCW (XY x y) = XY y (-x)

rotateCCW :: XY -> XY
rotateCCW (XY x y) = XY (-y) x

invert :: XY -> XY
invert (XY x y) = XY (-x) (-y)

freshGround :: XY -> MazeSeed -> Bool
freshGround delta maze = canAdvance delta maze && unsafeIsWall (delta `plus` delta) maze

findWayBack :: XY -> MazeSeed -> XY
findWayBack firstAttempt maze
  | not $ unsafeIsWall (rotateCW firstAttempt) maze   = rotateCW firstAttempt
  | not $ unsafeIsWall (rotateCCW firstAttempt) maze  = rotateCCW firstAttempt
  | not $ unsafeIsWall (invert firstAttempt) maze     = invert firstAttempt
  | otherwise = error "Can't find my way back!" -- This should never happen

intToDelta :: Int -> XY
intToDelta 0 = XY (-1) 0
intToDelta 1 = XY 0 (-1)
intToDelta 2 = XY 1 0
intToDelta 3 = XY 0 1
intToDelta i = error $ "Cannot map " #| i |# " to delta!"

isInsideDim :: XY -> XY -> Bool
isInsideDim (XY x y) (XY width height) = x >= 0 && x < width && y >= 0 && y < height

isInside :: XY -> Maze -> Bool
isInside pos maze = pos `isInsideDim` dim maze

step :: XY -> Maze -> Maze
step delta maze = if isValid then maze { playerPos = newPos } else maze where
  newPos = delta `plus` playerPos maze
  isValid = notOnWall && newPos `isInside` maze
  notOnWall = (board maze ! newPos) /= Wall

data Tick = Tick
type Name = ()

mazeApp :: App Maze Tick Name
mazeApp = App { appDraw = drawMaze
              , appChooseCursor = neverShowCursor
              , appHandleEvent = handleEvent
              , appStartEvent = return
              , appAttrMap = const mazeAttrMap
              }

drawMaze :: Maze -> [Widget Name]
drawMaze (Maze mazeStuff (XY width height) (XY guyX guyY)) = [C.center mazeWidget] where
  mazeWidget = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (withAttr mazeAttr $ str " MAZE ")
    $ vBox rows
  rows = [hBox $ cells y | y <- [0..height-1]]
  cells y = [drawCoord (x,y) | x <- [0..width-1]]
  drawCoord (x,y) = if x == guyX && y == guyY then withAttr guyAttr basicCell else
    case mazeStuff ! XY x y of
      Floor -> withAttr floorAttr basicCell
      Wall  -> withAttr wallAttr basicCell

basicCell :: Widget Name
basicCell = str "  "

mazeAttr, wallAttr, floorAttr, guyAttr :: AttrName
mazeAttr = "maze"
wallAttr = "wall"
floorAttr = "floor"
guyAttr = "guy"

mazeAttrMap :: AttrMap
mazeAttrMap = attrMap V.defAttr
  [ (mazeAttr, fg V.cyan)
  , (wallAttr, V.blue `on` V.blue)
  , (floorAttr, V.green `on` V.green)
  , (guyAttr, V.magenta `on` V.magenta) ]

handleEvent :: Maze -> BrickEvent Name Tick -> EventM Name (Next Maze)
handleEvent maze (VtyEvent (V.EvKey V.KUp []))     = continue $ step (XY 0 (-1)) maze
handleEvent maze (VtyEvent (V.EvKey V.KDown []))   = continue $ step (XY 0 1) maze
handleEvent maze (VtyEvent (V.EvKey V.KRight []))  = continue $ step (XY 1 0) maze
handleEvent maze (VtyEvent (V.EvKey V.KLeft []))   = continue $ step (XY (-1) 0) maze
handleEvent maze (VtyEvent (V.EvKey V.KEsc []))    = halt maze
handleEvent maze _ = continue maze
