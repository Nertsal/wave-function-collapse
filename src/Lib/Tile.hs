module Lib.Tile (tileMatches, allTileOrientations) where

import qualified Data.Maybe as Maybe
import qualified Lib.Direction as Direction
import Lib.Types

-- | Returns a dictionary of matching tiles for each side of a tile.
tileMatches :: Assets -> TileType -> [(Direction, [Tile])]
tileMatches assets tile = matchingTiles assets . Maybe.fromJust $ tile `lookup` assetTileConnections assets

-- | Given a list of connections of a tile, returns a list of matching tiles for every side.
matchingTiles :: Assets -> [[Connection]] -> [(Direction, [Tile])]
matchingTiles assets connections =
  map
    (matchConnection assets . (\dir -> (dir, getSideConnections dir connections)))
    Direction.allDirections

-- | Given a list of connections for a single side, returns a list of matching tiles for that side.
matchConnection :: Assets -> (Direction, [Connection]) -> (Direction, [Tile])
matchConnection assets (direction, connections) = (direction, filter checkTile (allTileOrientations assets))
  where
    checkTile tile =
      let dir = Direction.rotate (Direction.negative (tileDirection tile)) (Direction.opposite direction)
       in connections
            == ( getSideConnections dir
                   . Maybe.fromJust
                   $ (tileType tile `lookup` assetTileConnections assets)
               )

-- | From a list of all connections, return connections for a specific side
getSideConnections :: Direction -> [[Connection]] -> [Connection]
getSideConnections side connections =
  let i = Direction.directionIndex side
   in connections !! i

allTileTypes :: Assets -> [TileType]
allTileTypes assets = map fst (assetTiles assets)

allTileOrientations :: Assets -> [Tile]
allTileOrientations assets =
  concatMap
    ( \typ ->
        map (\direction -> Tile {tileDirection = direction, tileType = typ}) Direction.allDirections
    )
    (allTileTypes assets)
