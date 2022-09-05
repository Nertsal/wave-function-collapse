{-# LANGUAGE TupleSections #-}

module Lib.WFC (newWFC, drawWFC, genNextTile) where

import Control.Monad.Random (Rand, RandomGen, getRandomR, uniform)
import qualified Data.Bifunctor
import Data.List (intersect, sortBy)
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import qualified Graphics.Gloss as Gloss
import qualified Lib.Constants as Constants
import qualified Lib.Direction as Direction
import qualified Lib.Tile as Tile
import Lib.Types

newWFC :: (Int, Int) -> WFC
newWFC (width, height) =
  WFC
    { wfcWidth = width,
      wfcHeight = height,
      wfcTiles = Vector.replicate (width * height) Nothing,
      wfcHistory = mempty
    }

drawWFC :: Assets -> WFC -> IO Gloss.Picture
drawWFC assets wfc = do
  let width = wfcWidth wfc
      height = wfcHeight wfc
  return $ wfcLines width height <> wfcCells assets width height (wfcTiles wfc)

wfcCells :: Assets -> Int -> Int -> Vector (Maybe Tile) -> Gloss.Picture
wfcCells assets width height tiles = Gloss.pictures pictures
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

wfcLines :: Int -> Int -> Gloss.Picture
wfcLines width height =
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
    (Tile.allTileOrientations assets)
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

inBounds :: Int -> Int -> WFC -> Bool
inBounds x y wfc = x >= 0 && x < width && y >= 0 && y < height
  where
    width = wfcWidth wfc
    height = wfcHeight wfc

getNeighbourIndices :: Int -> WFC -> [(Direction, Int)]
getNeighbourIndices index wfc =
  map (\(dir, x, y) -> (dir, x + y * width))
    . filter (\(_, x, y) -> inBounds x y wfc)
    $ [(DirUp, tileX, tileY + 1), (DirRight, tileX + 1, tileY), (DirDown, tileX, tileY - 1), (DirLeft, tileX - 1, tileY)]
  where
    width = wfcWidth wfc
    tileX = index `mod` width
    tileY = index `div` width

getNeighbours :: Int -> WFC -> [(Direction, Tile)]
getNeighbours index wfc =
  Maybe.mapMaybe
    ( \(dir, i) ->
        fmap (dir,) (wfcTiles wfc ! i)
    )
    (getNeighbourIndices index wfc)

genNextTile :: (RandomGen g) => Assets -> WFC -> Bool -> Rand g WFC
genNextTile assets wfc autoRestart = do
  let toGen = generateOptions assets wfc
  -- Auto restart when some tile has no possible generation options
  if not (null toGen) && (null . snd . head) toGen
    then if autoRestart then rollback assets wfc else return wfc
    else applyRandomOption toGen wfc

-- | Generates valid options for empty tile and sorts the resulting list.
generateOptions :: Assets -> WFC -> [(Int, [Tile])]
generateOptions assets wfc =
  sortBy (\(_, a) (_, b) -> length a `compare` length b)
    . map (\(i, _) -> (i, genOptions assets (getNeighbours i wfc)))
    . filter (Maybe.isNothing . snd)
    . zip [0 ..]
    . Vector.toList
    $ wfcTiles wfc

-- | Keeps the front elements with equal "value".
keepFront :: (Foldable t) => [(b, t a)] -> [(b, t a)]
keepFront [] = []
keepFront (x : xs) =
  let f = length . snd
      best = f x
   in takeWhile ((== best) . f) (x : xs)

chooseIndex :: (RandomGen g, Foldable t) => t a -> Rand g (Maybe Int)
chooseIndex options =
  if null options
    then return Nothing
    else do
      i <- getRandomR (0, length options - 1)
      return (Just i)

removeOption :: [(Int, [Tile])] -> Int -> [(Int, [Tile])]
removeOption options i = map (Data.Bifunctor.second (map snd . filter ((/= i) . fst) . zip [0 ..])) options

-- | Picks a random choice among the ones with the lowest entropy (the number of options).
applyRandomOption :: (RandomGen g) => [(Int, [Tile])] -> WFC -> Rand g WFC
applyRandomOption options wfc = do
  let gen = keepFront $ dropWhile (null . snd) options
  index <- chooseIndex gen
  case index of
    Nothing -> return wfc
    Just i -> applyOption (gen !! i) (removeOption gen i) wfc

-- | Applies the given option.
applyOption :: (RandomGen g) => (Int, [Tile]) -> [(Int, [Tile])] -> WFC -> Rand g WFC
applyOption (tileIndex, choices) otherOptions wfc =
  if null choices
    then return wfc
    else fmap (updateTile tileIndex otherOptions wfc) (uniform choices)

-- | Updates the tile and adds a new history entry.
updateTile :: Int -> [(Int, [Tile])] -> WFC -> Tile -> WFC
updateTile tileIndex otherOptions wfc newTile =
  let newTiles = wfcTiles wfc // [(tileIndex, Just newTile)]
      entry =
        HistoryEntry
          { entryTiles = wfcTiles wfc,
            entryOptions = otherOptions
          }
      newHistory = wfcHistory wfc `Vector.snoc` entry
   in wfc
        { wfcTiles = newTiles,
          wfcHistory = newHistory
        }

rollback :: (RandomGen g) => Assets -> WFC -> Rand g WFC
rollback assets wfc =
  let entry = Vector.last (wfcHistory wfc)
      wfc' = wfc {wfcTiles = entryTiles entry, wfcHistory = Vector.init (wfcHistory wfc)}
      toGen = entryOptions entry
   in if not (null toGen) && (null . snd . head) toGen
        then rollback assets wfc'
        else applyRandomOption toGen wfc'
