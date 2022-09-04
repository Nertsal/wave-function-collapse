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
import qualified Lib.Constants as Constants
import Lib.Types
import qualified System.Exit

data TileAsset = TileAsset
  { tilePath :: FilePath,
    tileConnections :: [[Connection]]
  }
  deriving (Generic, Show, Aeson.FromJSON)

loadAssets :: IO Assets
loadAssets = do
  contents <- BS.readFile "static/tiles.json"
  let result = Aeson.eitherDecode contents :: Either String [(TileType, TileAsset)]
  case result of
    Left err -> System.Exit.die err
    Right tileAsset -> do
      tiles <- mapM (\(tile, asset) -> do _ <- validate asset; picture <- loadTilePicture asset; return (tile, picture)) tileAsset
      let connections = map (Data.Bifunctor.second tileConnections) tileAsset
      return Assets {assetTiles = tiles, assetTileConnections = connections}
  where
    process :: Maybe Gloss.Picture -> Gloss.Picture
    process = Gloss.scale 4.5 4.5 . Maybe.fromJust

    loadTilePicture :: TileAsset -> IO Gloss.Picture
    loadTilePicture asset = do
      picture <- Juicy.loadJuicyPNG (tilePath asset)
      return (process picture)

-- | Checks tile asset for validity: the number of connections
validate :: TileAsset -> IO TileAsset
validate a = validate' (tileConnections a) a
  where
    validate' :: [[Connection]] -> TileAsset -> IO TileAsset
    validate' [] asset = return asset
    validate' (cs : css) asset = do
      let expected = Constants.connectionsPerSide
      let actual = length cs
      if expected /= actual
        then
          System.Exit.die
            ( "The number of connections per side for the tile "
                ++ show (tilePath asset)
                ++ " did not match the expected value "
                ++ show expected
                ++ ". Actual value is "
                ++ show actual
            )
        else validate' css asset
