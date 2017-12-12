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
import qualified System.Random              as Rand

start :: IO ()
start = do
  gen <- Rand.getStdGen
  maze <- genMaze 10 10 gen
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

type MazeSeed = (IOArr.IOArray XY Space, XY, Integer, [XY])

genMaze :: Int -> Int -> Rand.StdGen -> IO Maze
genMaze width height gen = do
  let (genForOtherThings, dirGen) = Rand.split gen
  let randomDeltas = map intToDelta $ Rand.randomRs (0 :: Int, 3) dirGen
  let dim = XY width height
  let startingPos = XY 1 1
  floorplan <- IOArr.newArray (XY 0 0, dim) Wall :: IO (IOArr.IOArray XY Space)
  MArr.writeArray floorplan startingPos Floor
  let mazeSeed = (floorplan, startingPos, 0, randomDeltas)
  -- Loop.iterateUntilM goodMaze buildMaze mazeSeed
  -- let validSquares = zip [1, 3 .. width] [1, 3 .. height]
  -- TODO: Find some notion of whether or not a valid square can exit the maze
  -- TODO: Fill in the dead-ends in the rest of the maze
  frozenFloorplan <- MArr.freeze floorplan
  return $ Maze frozenFloorplan dim startingPos

goodMaze :: MazeSeed -> Bool
goodMaze = undefined --Check if maze can exit in one step with a total path length that's above threshold

buildMaze :: MazeSeed-> IO MazeSeed
buildMaze = undefined
-- nextPos <- pure $ clearingPos `plus` nextDelta `plus` nextDelta
-- if nextPos `isInsideDim` dim then do  --- TODO Also check to see if that square has been cleared previously
--   pathLength <- pure (pathLength + 2)
--   MArr.writeArray floorplan nextPos Floor
--   MArr.writeArray floorplan (clearingPos `plus` nextDelta) Floor
-- else do
--   return ()

intToDelta :: Int -> XY
intToDelta 0 = XY (-1) 0
intToDelta 1 = XY 0 (-1)
intToDelta 2 = XY 1 0
intToDelta 3 = XY 1 1
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
