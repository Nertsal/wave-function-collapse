{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Assets (Assets (..), loadAssets) where

import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Juicy as Juicy
import Lib.Types
import qualified System.Exit

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
