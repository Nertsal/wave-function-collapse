module Lib
  ( run,
  )
where

import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen, uniform)
import qualified Control.Monad.Random as Random
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

run :: IO ()
run = Gloss.playIO display background fps grid drawGrid handleEvent updateWorld
  where
    fps = 60
    display = Gloss.FullScreen
    background = Gloss.black
    grid = newGrid 10 10

handleEvent :: Gloss.Event -> Grid -> IO Grid
handleEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) Gloss.Down _ _) grid = Random.evalRandIO $ genNextTile grid
handleEvent _ grid = return grid

updateWorld :: Float -> Grid -> IO Grid
updateWorld _ = return

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
      gridTiles = Vector.replicate (width * height) Nothing
    }

drawGrid :: Grid -> IO Gloss.Picture
drawGrid grid = do
  let width = gridWidth grid
      height = gridHeight grid
  return $ gridLines width height <> gridCells width height (gridTiles grid)

gridCells :: Int -> Int -> Vector (Maybe Tile) -> Gloss.Picture
gridCells width height tiles = Gloss.pictures pictures
  where
    (tileWidth, tileHeight) = tileSize
    position i = (i `mod` width, i `div` width)
    indexed = [0 ..] `zip` Vector.toList tiles
    pictures = map drawTile indexed
    drawTile (_, Nothing) = Gloss.blank
    drawTile (index, Just tile) =
      let (x, y) = position index
          dx = (fromIntegral x - fromIntegral width / 2.0 + 0.5) * tileWidth
          dy = (fromIntegral y - fromIntegral height / 2.0 + 0.5) * tileHeight
       in Gloss.translate dx dy (tilePicture tile)

tilePicture :: Tile -> Gloss.Picture
tilePicture tile =
  let (w, h) = tileSize
      bg = Gloss.color (Gloss.greyN 0.5) $ Gloss.rectangleSolid w h
   in case tile of
        Empty -> bg
        Horizontal -> bg <> Gloss.color Gloss.blue (Gloss.rectangleSolid w (h * 0.3))
        Vertical -> bg <> Gloss.color Gloss.blue (Gloss.rectangleSolid (w * 0.3) h)

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

genNextTile :: (RandomGen g) => Grid -> Rand g Grid
genNextTile grid = do
  let toGen = Vector.map fst . Vector.filter (Maybe.isNothing . snd) . Vector.indexed $ gridTiles grid
  -- TODO: generate candidates for each tile
  i <- getRandomR (0, length toGen - 1)
  newTile <- uniform [Empty, Horizontal, Vertical]
  let newTiles = gridTiles grid // [(toGen ! i, Just newTile)]
  return grid {gridTiles = newTiles}
