{-# LANGUAGE TupleSections #-}

module Lib (run) where

import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen, uniform)
import qualified Control.Monad.Random as Random
import Data.List (intersect, sortBy)
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Prelude hiding (Left, Right)

run :: IO ()
run = Gloss.playIO display background fps grid drawGrid handleEvent updateWorld
  where
    fps = 60
    display = Gloss.FullScreen
    background = Gloss.black
    grid = newGrid 10 10

-- | Returns a dictionary of matching tiles for each side of a tile.
tileMatches :: TileType -> [(Direction, [Tile])]
tileMatches tile = matchingTiles . Maybe.fromJust $ tile `lookup` tileConnections
  where
    matchingTiles :: [(Direction, Bool)] -> [(Direction, [Tile])]
    matchingTiles = map matchConnection

    matchConnection :: (Direction, Bool) -> (Direction, [Tile])
    matchConnection (direction, isConnected) = (direction, filter checkTile allTileOrientations)
      where
        checkTile tile' =
          let dir = rotateDirection (negativeDirection (tileDirection tile')) (oppositeDirection direction)
           in isConnected
                == ( Maybe.fromJust
                       . lookup dir
                       . Maybe.fromJust
                       $ (tileType tile' `lookup` tileConnections)
                   )

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

data Tile = Tile
  { tileDirection :: Direction,
    tileType :: TileType
  }
  deriving (Show, Eq)

data Direction = Up | Right | Down | Left deriving (Show, Eq)

data TileType = Empty | Straight | Tri deriving (Show, Eq)

allDirections :: [Direction]
allDirections = [Up, Right, Down, Left]

allTileTypes :: [TileType]
allTileTypes = [Empty, Straight, Tri]

-- | A dictionary of tile connections if the default orientation (upwards).
tileConnections :: [(TileType, [(Direction, Bool)])]
tileConnections =
  [ ( Empty,
      [ (Up, False),
        (Right, False),
        (Down, False),
        (Left, False)
      ]
    ),
    ( Straight,
      [ (Up, False),
        (Right, True),
        (Down, False),
        (Left, True)
      ]
    ),
    ( Tri,
      [ (Up, True),
        (Right, True),
        (Down, False),
        (Left, True)
      ]
    )
  ]

allTileOrientations :: [Tile]
allTileOrientations =
  concatMap
    ( \typ ->
        map (\direction -> Tile {tileDirection = direction, tileType = typ}) allDirections
    )
    allTileTypes

directionRotation :: Direction -> Float
directionRotation Up = 0
directionRotation Right = 90
directionRotation Down = 180
directionRotation Left = 270

oppositeDirection :: Direction -> Direction
oppositeDirection Up = Down
oppositeDirection Right = Left
oppositeDirection Down = Up
oppositeDirection Left = Right

negativeDirection :: Direction -> Direction
negativeDirection Up = Up
negativeDirection Right = Left
negativeDirection Down = Down
negativeDirection Left = Right

rotateDirection :: Direction -> Direction -> Direction
rotateDirection a b = getRotation ((rotationIndex a + rotationIndex b :: Int) `mod` 4)
  where
    rotationIndex dir = case dir of
      Up -> 0
      Right -> 1
      Down -> 2
      Left -> 3
    getRotation index = case index of
      0 -> Up
      1 -> Right
      2 -> Down
      3 -> Left
      _ -> undefined

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
      rotation = directionRotation (tileDirection tile)
      picture = case tileType tile of
        Empty -> Gloss.blank
        Straight -> Gloss.color Gloss.blue (Gloss.rectangleSolid w (h * 0.3))
        Tri -> Gloss.color Gloss.blue (Gloss.rectangleSolid w (h * 0.3) <> Gloss.translate 0 (h * 0.25) (Gloss.rectangleSolid (w * 0.3) (h * 0.5)))
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

genOptions :: [(Direction, Tile)] -> [Tile]
genOptions neighbours =
  foldl
    intersect
    allTileOrientations
    ( map
        ( \(dir, tile) ->
            map
              (\newTile -> newTile {tileDirection = rotateDirection (tileDirection newTile) (tileDirection tile)})
              ( Maybe.fromJust
                  ( let direction = rotateDirection (negativeDirection (tileDirection tile)) (oppositeDirection dir)
                     in direction `lookup` tileMatches (tileType tile)
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
    $ [(Up, tileX, tileY + 1), (Right, tileX + 1, tileY), (Down, tileX, tileY - 1), (Left, tileX - 1, tileY)]
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

genNextTile :: (RandomGen g) => Grid -> Rand g Grid
genNextTile grid = do
  let toGen =
        dropWhile (null . snd)
          . sortBy (\(_, a) (_, b) -> length a `compare` length b)
          . map (\(i, _) -> (i, genOptions $ getNeighbours i grid))
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
