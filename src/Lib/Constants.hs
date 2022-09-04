module Lib.Constants (worldSize, tileSize, lineWidth, connectionsPerSide) where

worldSize :: (Int, Int)
worldSize = (20, 20)

tileSize :: (Float, Float)
tileSize = (50, 50)

lineWidth :: Float
lineWidth = 2

connectionsPerSide :: Int
connectionsPerSide = 3
