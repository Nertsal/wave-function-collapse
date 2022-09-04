{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Types (World (..), Assets (..), Grid (..), Tile (..), TileType (..), Direction (..)) where

import qualified Data.Aeson as Aeson
import Data.Vector (Vector)
import GHC.Generics (Generic)
import qualified Graphics.Gloss as Gloss

data World = World
  { worldAssets :: Assets,
    worldContinuousGen :: Bool,
    worldGrid :: Grid
  }

data Assets = Assets
  { assetTiles :: [(TileType, Gloss.Picture)],
    assetTileConnections :: [(TileType, [Direction])]
  }

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

data TileType = TileEmpty | TileStraight | TileTri | TileTurn deriving (Generic, Show, Eq, Read, Aeson.FromJSON)

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Show, Eq, Generic, Aeson.FromJSON)
