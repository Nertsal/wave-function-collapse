module Lib.Constants (worldSize, tileSize, lineWidth, connectionsPerSide) where

worldSize :: (Int, Int)
worldSize = (50, 50)

tileSize :: (Float, Float)
tileSize = (20, 20)

lineWidth :: Float
lineWidth = 2

connectionsPerSide :: Int
connectionsPerSide = 3
