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
import           Data.Foldable              (sequence_)
import           Fmt                        (( #| ), (|#))
import qualified Gen
import qualified Graphics.Vty               as V
import           MazeCore
import           System.IO.Unsafe           (unsafePerformIO)
import qualified System.Random              as Rand
import           System.Random.Shuffle      (shuffleM)

start :: IO ()
start = do
  gen <- Rand.getStdGen
  let mazeSize = 11
  maze <- Gen.genMaze mazeSize mazeSize gen
  finalState <- defaultMain mazeApp maze
  return ()

step :: XY -> Maze -> Maze
step delta maze = if isValid then maze { playerPos = newPos } else maze where
  newPos = delta `plus` playerPos maze
  -- Important that 'notOnWall' comes second so it can short-circuit before crashing
  isValid = newPos `isInside` maze  && notOnWall
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
