{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Types (World (..), Assets (..), WFC (..), Tile (..), TileType, Direction (..), Connection) where

import qualified Data.Aeson as Aeson
import Data.Vector (Vector)
import GHC.Generics (Generic)
import qualified Graphics.Gloss as Gloss

data World = World
  { worldAssets :: Assets,
    worldAutoRestart :: Bool,
    worldContinuousGen :: Bool,
    worldWFC :: WFC
  }

data Assets = Assets
  { assetTiles :: [(TileType, Gloss.Picture)],
    assetTileConnections :: [(TileType, [[Connection]])]
  }

data WFC = WFC
  { wfcWidth :: Int,
    wfcHeight :: Int,
    wfcTiles :: Vector (Maybe Tile)
  }

data Tile = Tile
  { tileDirection :: Direction,
    tileType :: TileType
  }
  deriving (Show, Eq)

type TileType = String

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Show, Eq, Generic, Aeson.FromJSON)

type Connection = Int
