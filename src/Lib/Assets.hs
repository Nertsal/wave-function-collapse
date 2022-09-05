{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Assets (Assets (..), loadAssets) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Juicy as Juicy
import qualified Lib.Constants as Constants
import Lib.Types
import qualified System.Exit
import System.FilePath ((</>))

data TileAsset = TileAsset
  { tilePath :: FilePath,
    tileConnections :: [[Connection]]
  }
  deriving (Generic, Show, Aeson.FromJSON)

loadAssets :: FilePath -> IO Assets
loadAssets tileMapPath = do
  contents <- BS.readFile (tileMapPath </> "tiles.json")
  let result = Aeson.eitherDecode contents :: Either String [TileAsset]
  case result of
    Left err -> System.Exit.die err
    Right tempAssets -> do
      let expectedConnections = case tempAssets of
            (TileAsset {tileConnections = (cons : _)} : _) -> length cons
            _ -> 0
      tiles' <- mapM (\asset -> do _ <- validate expectedConnections asset; picture <- loadTilePicture asset; return (tilePath asset, picture)) tempAssets
      let connections = map (\asset -> (tilePath asset, tileConnections asset)) tempAssets
      return
        Assets
          { assetTiles = tiles',
            assetTileConnections = connections
          }
  where
    process :: Maybe Gloss.Picture -> Gloss.Picture
    process picture = case Maybe.fromJust picture of
      pic@(Gloss.Bitmap bitmap) ->
        let (w, h) = Gloss.bitmapSize bitmap
            (tx, ty) = Constants.tileSize
            (x, y) = (tx / fromIntegral w, ty / fromIntegral h)
         in Gloss.scale x y pic
      pic -> pic

    loadTilePicture :: TileAsset -> IO Gloss.Picture
    loadTilePicture asset = do
      picture <- Juicy.loadJuicyPNG (tileMapPath </> tilePath asset)
      return (process picture)

-- | Checks tile asset for validity: the number of connections
validate :: Int -> TileAsset -> IO TileAsset
validate expected a = validate' (tileConnections a) a
  where
    validate' :: [[Connection]] -> TileAsset -> IO TileAsset
    validate' [] asset = return asset
    validate' (cs : css) asset = do
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
