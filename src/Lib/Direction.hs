module Lib.Direction (allDirections, rotation, opposite, negative, rotate, directionIndex) where

import Lib.Types (Direction (..))

allDirections :: [Direction]
allDirections = [DirUp, DirRight, DirDown, DirLeft]

rotation :: Direction -> Float
rotation DirUp = 0
rotation DirRight = 90
rotation DirDown = 180
rotation DirLeft = 270

opposite :: Direction -> Direction
opposite DirUp = DirDown
opposite DirRight = DirLeft
opposite DirDown = DirUp
opposite DirLeft = DirRight

negative :: Direction -> Direction
negative DirUp = DirUp
negative DirRight = DirLeft
negative DirDown = DirDown
negative DirLeft = DirRight

rotate :: Direction -> Direction -> Direction
rotate a b = getRotation ((directionIndex a + directionIndex b :: Int) `mod` 4)

directionIndex :: Num p => Direction -> p
directionIndex dir = case dir of
  DirUp -> 0
  DirRight -> 1
  DirDown -> 2
  DirLeft -> 3

getRotation :: (Eq a, Num a) => a -> Direction
getRotation index = case index of
  0 -> DirUp
  1 -> DirRight
  2 -> DirDown
  3 -> DirLeft
  _ -> undefined
