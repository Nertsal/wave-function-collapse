module Lib (run) where

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Lib.Assets (loadAssets)
import Lib.World

run :: IO ()
run = do
  assets <- loadAssets
  let world = initialize assets
  Gloss.playIO display background fps world renderWorld handleEvent updateWorld
  where
    fps = 120
    display = Gloss.FullScreen
    background = Gloss.black
