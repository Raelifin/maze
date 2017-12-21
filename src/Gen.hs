module Gen where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Control.Monad.Loops        as Loop
import           Data.Array.IArray          ((!))
import qualified Data.Array.IArray          as IArr
import qualified Data.Array.IO              as IOArr
import qualified Data.Array.MArray          as MArr
import           Data.Foldable              (sequence_)
import           Fmt                        (( #| ), (|#))
import qualified Graphics.Vty               as V
import           MazeCore
import           System.IO.Unsafe           (unsafePerformIO)
import qualified System.Random              as Rand
import           System.Random.Shuffle      (shuffleM)

type MazeSeed = (IOArr.IOArray XY Space, IOArr.IOArray XY Bool, XY, XY, Int, [XY])

genMaze :: Int -> Int -> Rand.StdGen -> IO Maze
genMaze width height gen = do
  let (genForOtherThings, dirGen) = Rand.split gen
  let randomDeltas = map intToDelta $ Rand.randomRs (0 :: Int, 3) dirGen
  let dim = XY width height
  let startingPos = XY 1 1
  floorplan <- IOArr.newArray (XY 0 0, dim) Wall :: IO (IOArr.IOArray XY Space)
  canExit <- IOArr.newArray (XY 0 0, dim) False :: IO (IOArr.IOArray XY Bool)
  MArr.writeArray floorplan startingPos Floor
  MArr.writeArray canExit startingPos True
  let mazeSeed = (floorplan, canExit, dim, startingPos, 0, randomDeltas)
  (_, _, _, endPos, _, randomDeltas) <- Loop.iterateUntilM goodMaze buildMaze mazeSeed
  breakOut endPos floorplan dim
  -- A "room" is different from a "corridor" in that it can have multiple exits
  let rooms = XY <$> [1, 3 .. (width-1)] <*> [1, 3 .. (height-1)]
  clearAll rooms floorplan
  shuffledRooms <- shuffleM rooms
  Loop.concatM (connectToMainPath floorplan canExit dim <$> shuffledRooms) randomDeltas
  frozenFloorplan <- MArr.freeze floorplan
  return $ Maze frozenFloorplan dim startingPos

type ConnectingGrp = (IOArr.IOArray XY Space, IOArr.IOArray XY Bool, XY, [XY], [XY])

connectToMainPath :: IOArr.IOArray XY Space -> IOArr.IOArray XY Bool -> XY -> XY -> [XY] -> IO [XY]
connectToMainPath floorplan canExit dim pos randomDeltas = do
    (_, _, _, path, randomDeltas) <- Loop.iterateUntilM onCanExitSpace grow (floorplan, canExit, dim, [pos], randomDeltas)
    markPathAsCanExit path canExit
    return randomDeltas

markPathAsCanExit :: [XY] -> IOArr.IOArray XY Bool -> IO ()
markPathAsCanExit [] _ = return ()
markPathAsCanExit (p:ps) canExit = do
  MArr.writeArray canExit p True
  markPathAsCanExit ps canExit

onCanExitSpace :: ConnectingGrp -> Bool
onCanExitSpace (_, canExit, _, pos:_, _) = unsafePerformIO (MArr.readArray canExit pos)

grow :: ConnectingGrp -> IO ConnectingGrp
grow (floorplan, canExit, dim, pos : trail, nextDelta : deltas) = do
  let currentGroup = (floorplan, canExit, dim, pos:trail, deltas)
  let nextPos = pos `plus` nextDelta `plus` nextDelta
  if nextPos `isInsideDim` dim then do
    -- TODO: Theoretically we want to prevent the path from threading back on itself?
    MArr.writeArray floorplan (pos `plus` nextDelta) Floor -- Clear a path
    return (floorplan, canExit, dim, nextPos:(pos:trail), deltas)
  else
    return currentGroup -- Tried to make invalid move. Try again with a different delta.

-- | Breaks the final wall to make an exit to the maze
breakOut :: XY -> IOArr.IOArray XY Space -> XY -> IO ()
breakOut (XY x y) floorplan (XY width height)
  | x == 1          = MArr.writeArray floorplan (XY (x - 1) y) Floor
  | y == 1          = MArr.writeArray floorplan (XY x (y - 1)) Floor
  | x == width - 2  = MArr.writeArray floorplan (XY (x + 1) y) Floor
  | y == height - 2 = MArr.writeArray floorplan (XY x (y + 1)) Floor
  | otherwise       = error "Tried to break out, but no exit found!"

clearAll :: [XY] -> IOArr.IOArray XY Space -> IO (IOArr.IOArray XY Space)
clearAll [] floorplan = pure floorplan
clearAll (thisPos : otherPos) floorplan = do
  MArr.writeArray floorplan thisPos Floor
  clearAll otherPos floorplan

goodMaze :: MazeSeed -> Bool
goodMaze (_, _, dim@(XY x y), pos, pathLength, _) = pathLength > x + y && canExitInOneStep dim pos

pathLengthThreshold :: Int
pathLengthThreshold = 21

canExitInOneStep :: XY -> XY -> Bool
canExitInOneStep (XY width height) (XY x y) = x < 2 || y < 2 || x > width - 3 || y > height - 3

buildMaze :: MazeSeed-> IO MazeSeed
buildMaze (floorplan, canExit, dim, pos, pathLength, nextDelta : deltas)
  | freshGround nextDelta maze = advance nextDelta maze
  | canAdvance nextDelta maze && unsafeIsWall nextDelta maze
    = either (`advance` maze) (`backOut` maze) (searchForAlternativeRoutes nextDelta maze)
  | otherwise = return maze  -- We tried to make an invalid move. Try again, ignoring this delta.
  where maze = (floorplan, canExit, dim, pos, pathLength, deltas)

-- | Uses unsafePerformIO to check whether a relative position on our floorplan is a Wall
unsafeIsWall :: XY -> MazeSeed -> Bool
unsafeIsWall delta (floorplan, _, _, pos, _, _)
  = unsafePerformIO (MArr.readArray floorplan (pos `plus` delta)) == Wall

-- | Checks whether we're about to fall off the map
canAdvance :: XY -> MazeSeed -> Bool
canAdvance nextDelta (_, _, dim, pos, _, _) = nextPos `isInsideDim` dim where
  nextPos = pos `plus` nextDelta `plus` nextDelta

-- | Warning: Unsafe. Use with caution.
advance :: XY -> MazeSeed -> IO MazeSeed
advance nextDelta (floorplan, canExit, dim, pos, pathLength, deltas) = do
  nextPos <- pure $ pos `plus` nextDelta `plus` nextDelta
  MArr.writeArray floorplan nextPos Floor
  MArr.writeArray floorplan (pos `plus` nextDelta) Floor
  MArr.writeArray canExit nextPos True
  MArr.writeArray canExit (pos `plus` nextDelta) True
  return (floorplan, canExit, dim, nextPos, pathLength + 2, deltas)

-- | Warning: Unsafe. Use with caution.
backOut :: XY -> MazeSeed -> IO MazeSeed
backOut wayBack (floorplan, canExit, dim, pos, pathLength, deltas) = do
  MArr.writeArray canExit pos False
  MArr.writeArray canExit (pos `plus` wayBack) False
  -- Build a wall behind us to keep it unambiguous where we've been.
  MArr.writeArray floorplan (pos `plus` wayBack) Wall
  -- Return to previous position
  return (floorplan, canExit, dim, pos `plus` wayBack `plus` wayBack, pathLength - 2, deltas)

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
intToDelta i = error $ "Cannot map " ++ show i ++ " to delta!"
