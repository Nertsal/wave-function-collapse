{-# LANGUAGE TupleSections #-}

module Lib.Grid (newGrid, drawGrid, genNextTile) where

import Control.Monad.Random (Rand, RandomGen, getRandomR, uniform)
import Data.List (intersect, sortBy)
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import qualified Graphics.Gloss as Gloss
import qualified Lib.Constants as Constants
import qualified Lib.Direction as Direction
import qualified Lib.Tile as Tile
import Lib.Types

newGrid :: Int -> Int -> Grid
newGrid width height =
  Grid
    { gridWidth = width,
      gridHeight = height,
      gridTiles = Vector.replicate (width * height) Nothing
    }

drawGrid :: Assets -> Grid -> IO Gloss.Picture
drawGrid assets grid = do
  let width = gridWidth grid
      height = gridHeight grid
  return $ gridLines width height <> gridCells assets width height (gridTiles grid)

gridCells :: Assets -> Int -> Int -> Vector (Maybe Tile) -> Gloss.Picture
gridCells assets width height tiles = Gloss.pictures pictures
  where
    (tileWidth, tileHeight) = Constants.tileSize
    position i = (i `mod` width, i `div` width)
    indexed = [0 ..] `zip` Vector.toList tiles
    pictures = map drawTile indexed
    drawTile (_, Nothing) = Gloss.blank
    drawTile (index, Just tile) =
      let (x, y) = position index
          dx = (fromIntegral x - fromIntegral width / 2.0 + 0.5) * tileWidth
          dy = (fromIntegral y - fromIntegral height / 2.0 + 0.5) * tileHeight
       in Gloss.translate dx dy (tilePicture assets tile)

tilePicture :: Assets -> Tile -> Gloss.Picture
tilePicture assets tile =
  let (w, h) = Constants.tileSize
      bg = Gloss.color (Gloss.greyN 0.5) $ Gloss.rectangleSolid w h
      rotation = Direction.rotation (tileDirection tile)
      picture = Maybe.fromJust (tileType tile `lookup` assetTiles assets)
   in bg <> Gloss.rotate rotation picture

gridLines :: Int -> Int -> Gloss.Picture
gridLines width height =
  Gloss.color (Gloss.greyN 0.5)
    . Gloss.pictures
    $ map column [0 .. width] ++ map row [0 .. height]
  where
    w = fromIntegral width
    h = fromIntegral height
    (tileWidth, tileHeight) = Constants.tileSize
    column x = Gloss.translate ((fromIntegral x - w / 2.0) * tileWidth) 0 $ Gloss.rectangleSolid Constants.lineWidth (h * tileHeight)
    row y = Gloss.translate 0 ((fromIntegral y - h / 2.0) * tileHeight) $ Gloss.rectangleSolid (w * tileWidth) Constants.lineWidth

genOptions :: Assets -> [(Direction, Tile)] -> [Tile]
genOptions assets neighbours =
  foldl
    intersect
    Tile.allTileOrientations
    ( map
        ( \(dir, tile) ->
            map
              (\newTile -> newTile {tileDirection = Direction.rotate (tileDirection newTile) (tileDirection tile)})
              ( Maybe.fromJust
                  ( let direction = Direction.rotate (Direction.negative (tileDirection tile)) (Direction.opposite dir)
                     in direction `lookup` Tile.tileMatches assets (tileType tile)
                  )
              )
        )
        neighbours
    )

inBounds :: Int -> Int -> Grid -> Bool
inBounds x y grid = x >= 0 && x < width && y >= 0 && y < height
  where
    width = gridWidth grid
    height = gridHeight grid

getNeighbourIndices :: Int -> Grid -> [(Direction, Int)]
getNeighbourIndices index grid =
  map (\(dir, x, y) -> (dir, x + y * width))
    . filter (\(_, x, y) -> inBounds x y grid)
    $ [(DirUp, tileX, tileY + 1), (DirRight, tileX + 1, tileY), (DirDown, tileX, tileY - 1), (DirLeft, tileX - 1, tileY)]
  where
    width = gridWidth grid
    tileX = index `mod` width
    tileY = index `div` width

getNeighbours :: Int -> Grid -> [(Direction, Tile)]
getNeighbours index grid =
  Maybe.mapMaybe
    ( \(dir, i) ->
        fmap (dir,) (gridTiles grid ! i)
    )
    (getNeighbourIndices index grid)

genNextTile :: (RandomGen g) => Assets -> Grid -> Rand g Grid
genNextTile assets grid = do
  let toGen =
        dropWhile (null . snd)
          . sortBy (\(_, a) (_, b) -> length a `compare` length b)
          . map (\(i, _) -> (i, genOptions assets (getNeighbours i grid)))
          . filter (Maybe.isNothing . snd)
          . zip [0 ..]
          . Vector.toList
          $ gridTiles grid
  case toGen of
    [] -> return grid
    ((_, options) : _) -> do
      let candidates = takeWhile ((== length options) . length . snd) toGen
      i <- getRandomR (0, length candidates - 1)
      let (tileIndex, choices) = candidates !! i
      if null choices
        then return grid
        else do
          newTile <- uniform choices
          let newTiles = gridTiles grid // [(tileIndex, Just newTile)]
          return grid {gridTiles = newTiles}
