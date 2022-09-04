module Lib.World (initialize, handleEvent, updateWorld, renderWorld) where

import qualified Control.Monad.Random as Random
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Lib.Constants as Constants
import Lib.Types
import qualified Lib.WFC as WFC

initialize :: Assets -> World
initialize assets =
  World
    { worldAssets = assets,
      worldAutoRestart = False,
      worldContinuousGen = False,
      worldWFC = WFC.newWFC Constants.worldSize
    }

handleEvent :: Gloss.Event -> World -> IO World
handleEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) _ _ _) world = do
  return $ world {worldContinuousGen = not (worldContinuousGen world)}
handleEvent (Gloss.EventKey (Gloss.Char 'r') Gloss.Down _ _) world = do
  return $ world {worldWFC = WFC.newWFC Constants.worldSize}
handleEvent (Gloss.EventKey (Gloss.Char 'p') Gloss.Down _ _) world = do
  return $ world {worldAutoRestart = not (worldAutoRestart world)}
handleEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyEnter) Gloss.Down _ _) world = do
  wfc' <- Random.evalRandIO $ WFC.genNextTile (worldAssets world) (worldWFC world) True
  return $ world {worldWFC = wfc'}
handleEvent _ wfc = return wfc

updateWorld :: Float -> World -> IO World
updateWorld _ world =
  if worldContinuousGen world || worldAutoRestart world
    then do
      wfc' <- Random.evalRandIO $ WFC.genNextTile (worldAssets world) (worldWFC world) (worldAutoRestart world)
      return $ world {worldWFC = wfc'}
    else return world

renderWorld :: World -> IO Gloss.Picture
renderWorld world = WFC.drawWFC (worldAssets world) (worldWFC world)
