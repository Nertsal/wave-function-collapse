module Lib.Tile (tileMatches, allTileTypes, allTileOrientations) where

import qualified Data.Maybe as Maybe
import qualified Lib.Direction as Direction
import Lib.Types

-- | Returns a dictionary of matching tiles for each side of a tile.
tileMatches :: Assets -> TileType -> [(Direction, [Tile])]
tileMatches assets tile = matchingTiles . Maybe.fromJust $ tile `lookup` assetTileConnections assets
  where
    matchingTiles :: [Direction] -> [(Direction, [Tile])]
    matchingTiles connections = map (matchConnection . (\dir -> (dir, dir `elem` connections))) Direction.allDirections

    matchConnection :: (Direction, Bool) -> (Direction, [Tile])
    matchConnection (direction, isConnected) = (direction, filter checkTile allTileOrientations)
      where
        checkTile tile' =
          let dir = Direction.rotate (Direction.negative (tileDirection tile')) (Direction.opposite direction)
           in isConnected
                == ( elem dir
                       . Maybe.fromJust
                       $ (tileType tile' `lookup` assetTileConnections assets)
                   )

allTileTypes :: [TileType]
allTileTypes = [TileEmpty, TileStraight, TileTri, TileTurn]

allTileOrientations :: [Tile]
allTileOrientations =
  concatMap
    ( \typ ->
        map (\direction -> Tile {tileDirection = direction, tileType = typ}) Direction.allDirections
    )
    allTileTypes
