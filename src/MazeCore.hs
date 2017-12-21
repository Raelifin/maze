{-# LANGUAGE OverloadedStrings #-}

module MazeCore where

import qualified Data.Array.IArray as IArr
import qualified Data.Array.IO     as IOArr
import qualified Data.Array.MArray as MArr
import           Fmt

data XY = XY Int Int deriving (Eq, Ord, MArr.Ix)
instance Show XY where
  show (XY x y) = "(" #| x |# "," #| y |# ")"

asTuple :: XY -> (Int, Int)
asTuple (XY x y) = (x, y)

plus :: XY -> XY -> XY
plus (XY x1 y1) (XY x2 y2) = XY (x1+x2) (y1+y2)

data Maze = Maze { board :: IArr.Array XY Space, dim :: XY, playerPos :: XY }
data Space = Floor | Wall deriving (Eq, Show)

isInsideDim :: XY -> XY -> Bool
isInsideDim (XY x y) (XY width height) = x >= 0 && x < width && y >= 0 && y < height

isInside :: XY -> Maze -> Bool
isInside pos maze = pos `isInsideDim` dim maze
