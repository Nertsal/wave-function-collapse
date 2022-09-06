module Lib (run) where

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Lib.Assets (loadAssets)
import Lib.World
import System.Environment (getArgs)

run :: IO ()
run = do
  args <- getArgs
  let tileMapPath =
        case args of
          [] -> ""
          (path : _) -> path
  putStrLn ("Looking for a tile map in " ++ show tileMapPath)
  assets <- loadAssets tileMapPath
  let world = initialize assets
  Gloss.playIO display background fps world renderWorld handleEvent updateWorld
  where
    fps = 60
    display = Gloss.FullScreen
    background = Gloss.black
