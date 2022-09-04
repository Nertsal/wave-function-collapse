{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Lib (run) where

import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen, uniform)
import qualified Control.Monad.Random as Random
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import Data.List (intersect, sortBy)
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import GHC.Generics
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Graphics.Gloss.Juicy as Juicy
import qualified System.Exit

data World = World
  { worldAssets :: Assets,
    worldContinuousGen :: Bool,
    worldGrid :: Grid
  }

data Assets = Assets
  { assetTiles :: [(TileType, Gloss.Picture)],
    assetTileConnections :: [(TileType, [Direction])]
  }

initialize :: Assets -> World
initialize assets =
  World
    { worldAssets = assets,
      worldContinuousGen = False,
      worldGrid = newGrid 20 20
    }

run :: IO ()
run = do
  assets <- loadAssets
  let world = initialize assets
  Gloss.playIO display background fps world renderWorld handleEvent updateWorld
  where
    fps = 120
    display = Gloss.FullScreen
    background = Gloss.black

data TileAsset = TileAsset
  { tilePath :: FilePath,
    tileConnections :: [Direction]
  }
  deriving (Generic, Show, Aeson.FromJSON)

loadAssets :: IO Assets
loadAssets = do
  contents <- BS.readFile "static/tiles.json"
  let result = Aeson.eitherDecode contents :: Either String [(TileType, TileAsset)]
  case result of
    Left err -> System.Exit.die err
    Right tileAssets -> do
      tiles <- mapM (\(tile, asset) -> do picture <- loadTilePicture asset; return (tile, picture)) tileAssets
      let connections = map (Data.Bifunctor.second tileConnections) tileAssets
      return Assets {assetTiles = tiles, assetTileConnections = connections}
  where
    process :: Maybe Gloss.Picture -> Gloss.Picture
    process = Gloss.scale 4.5 4.5 . Maybe.fromJust

    loadTilePicture :: TileAsset -> IO Gloss.Picture
    loadTilePicture asset = do
      picture <- Juicy.loadJuicyPNG (tilePath asset)
      return (process picture)

-- | Returns a dictionary of matching tiles for each side of a tile.
tileMatches :: Assets -> TileType -> [(Direction, [Tile])]
tileMatches assets tile = matchingTiles . Maybe.fromJust $ tile `lookup` assetTileConnections assets
  where
    matchingTiles :: [Direction] -> [(Direction, [Tile])]
    matchingTiles connections = map (matchConnection . (\dir -> (dir, dir `elem` connections))) allDirections

    matchConnection :: (Direction, Bool) -> (Direction, [Tile])
    matchConnection (direction, isConnected) = (direction, filter checkTile allTileOrientations)
      where
        checkTile tile' =
          let dir = rotateDirection (negativeDirection (tileDirection tile')) (oppositeDirection direction)
           in isConnected
                == ( elem dir
                       . Maybe.fromJust
                       $ (tileType tile' `lookup` assetTileConnections assets)
                   )

handleEvent :: Gloss.Event -> World -> IO World
handleEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) _ _ _) world = do
  return $ world {worldContinuousGen = not (worldContinuousGen world)}
handleEvent (Gloss.EventKey (Gloss.Char 'r') Gloss.Down _ _) world = do
  return $ world {worldGrid = newGrid 20 20}
handleEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyEnter) Gloss.Down _ _) world = do
  grid' <- Random.evalRandIO $ genNextTile (worldAssets world) (worldGrid world)
  return $ world {worldGrid = grid'}
handleEvent _ grid = return grid

updateWorld :: Float -> World -> IO World
updateWorld _ world =
  if worldContinuousGen world
    then do
      grid' <- Random.evalRandIO $ genNextTile (worldAssets world) (worldGrid world)
      return $ world {worldGrid = grid'}
    else return world

renderWorld :: World -> IO Gloss.Picture
renderWorld world = drawGrid (worldAssets world) (worldGrid world)

tileSize :: (Float, Float)
tileSize = (50, 50)

lineWidth :: Float
lineWidth = 5

data Grid = Grid
  { gridWidth :: Int,
    gridHeight :: Int,
    gridTiles :: Vector (Maybe Tile)
  }

data Tile = Tile
  { tileDirection :: Direction,
    tileType :: TileType
  }
  deriving (Show, Eq)

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Show, Eq, Generic, Aeson.FromJSON)

data TileType = TileEmpty | TileStraight | TileTri | TileTurn deriving (Generic, Show, Eq, Read, Aeson.FromJSON)

allDirections :: [Direction]
allDirections = [DirUp, DirRight, DirDown, DirLeft]

allTileTypes :: [TileType]
allTileTypes = [TileEmpty, TileStraight, TileTri, TileTurn]

allTileOrientations :: [Tile]
allTileOrientations =
  concatMap
    ( \typ ->
        map (\direction -> Tile {tileDirection = direction, tileType = typ}) allDirections
    )
    allTileTypes

directionRotation :: Direction -> Float
directionRotation DirUp = 0
directionRotation DirRight = 90
directionRotation DirDown = 180
directionRotation DirLeft = 270

oppositeDirection :: Direction -> Direction
oppositeDirection DirUp = DirDown
oppositeDirection DirRight = DirLeft
oppositeDirection DirDown = DirUp
oppositeDirection DirLeft = DirRight

negativeDirection :: Direction -> Direction
negativeDirection DirUp = DirUp
negativeDirection DirRight = DirLeft
negativeDirection DirDown = DirDown
negativeDirection DirLeft = DirRight

rotateDirection :: Direction -> Direction -> Direction
rotateDirection a b = getRotation ((rotationIndex a + rotationIndex b :: Int) `mod` 4)
  where
    rotationIndex dir = case dir of
      DirUp -> 0
      DirRight -> 1
      DirDown -> 2
      DirLeft -> 3
    getRotation index = case index of
      0 -> DirUp
      1 -> DirRight
      2 -> DirDown
      3 -> DirLeft
      _ -> undefined

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
    (tileWidth, tileHeight) = tileSize
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
  let (w, h) = tileSize
      bg = Gloss.color (Gloss.greyN 0.5) $ Gloss.rectangleSolid w h
      rotation = directionRotation (tileDirection tile)
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
    (tileWidth, tileHeight) = tileSize
    column x = Gloss.translate ((fromIntegral x - w / 2.0) * tileWidth) 0 $ Gloss.rectangleSolid lineWidth (h * tileHeight)
    row y = Gloss.translate 0 ((fromIntegral y - h / 2.0) * tileHeight) $ Gloss.rectangleSolid (w * tileWidth) lineWidth

genOptions :: Assets -> [(Direction, Tile)] -> [Tile]
genOptions assets neighbours =
  foldl
    intersect
    allTileOrientations
    ( map
        ( \(dir, tile) ->
            map
              (\newTile -> newTile {tileDirection = rotateDirection (tileDirection newTile) (tileDirection tile)})
              ( Maybe.fromJust
                  ( let direction = rotateDirection (negativeDirection (tileDirection tile)) (oppositeDirection dir)
                     in direction `lookup` tileMatches assets (tileType tile)
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
