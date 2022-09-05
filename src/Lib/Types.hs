{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Types (World (..), Assets (..), WFC (..), HistoryEntry (..), Tile (..), TileType, Direction (..), Connection) where

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

-- | Wave Function Collapse
data WFC = WFC
  { wfcWidth :: Int,
    wfcHeight :: Int,
    wfcTiles :: Vector (Maybe Tile),
    -- | A history of every step. Used when backtracking
    wfcHistory :: Vector HistoryEntry
  }

data HistoryEntry = HistoryEntry
  { entryTiles :: Vector (Maybe Tile),
    entryOptions :: [(Int, [Tile])]
  }

data Tile = Tile
  { tileDirection :: Direction,
    tileType :: TileType
  }
  deriving (Show, Eq)

type TileType = String

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Show, Eq, Generic, Aeson.FromJSON)

type Connection = Char
