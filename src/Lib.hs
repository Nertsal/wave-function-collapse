module Lib
  ( run,
  )
where

import Data.Vector (Vector)
import qualified Graphics.Gloss as Gloss

tileSize :: (Float, Float)
tileSize = (50, 50)

lineWidth :: Float
lineWidth = 5

data Grid = Grid
  { gridWidth :: Int,
    gridHeight :: Int,
    gridTiles :: Vector (Maybe Tile)
  }

data Tile = Empty | Horizontal | Vertical

newGrid :: Int -> Int -> Grid
newGrid width height =
  Grid
    { gridWidth = width,
      gridHeight = height,
      gridTiles = mempty
    }

run :: IO ()
run = Gloss.display display background (drawGrid grid)
  where
    display = Gloss.FullScreen
    background = Gloss.black
    grid = newGrid 10 10

drawGrid :: Grid -> Gloss.Picture
drawGrid grid = gridLines (gridWidth grid) (gridHeight grid)

gridLines :: Int -> Int -> Gloss.Picture
gridLines width height =
  Gloss.color (Gloss.greyN 0.5)
    . Gloss.pictures
    $ map column [0 .. width] ++ map row [0 .. height]
  where
    w = fromIntegral width
    h = fromIntegral height
    (tileWidth, tileHeight) = tileSize
    column x = Gloss.translate ((fromIntegral x - w / 2.0) * tileWidth) 0 $ Gloss.rectangleSolid lineWidth (h * tileHeight)
    row y = Gloss.translate 0 ((fromIntegral y - h / 2.0) * tileHeight) $ Gloss.rectangleSolid (w * tileWidth) lineWidth
