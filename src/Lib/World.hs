module Lib.World (initialize, handleEvent, updateWorld, renderWorld) where

import qualified Control.Monad.Random as Random
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Lib.Grid as Grid
import Lib.Types

initialize :: Assets -> World
initialize assets =
  World
    { worldAssets = assets,
      worldAutoRestart = False,
      worldContinuousGen = False,
      worldGrid = Grid.newGrid 20 20
    }

handleEvent :: Gloss.Event -> World -> IO World
handleEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) _ _ _) world = do
  return $ world {worldContinuousGen = not (worldContinuousGen world)}
handleEvent (Gloss.EventKey (Gloss.Char 'r') Gloss.Down _ _) world = do
  return $ world {worldGrid = Grid.newGrid 20 20}
handleEvent (Gloss.EventKey (Gloss.Char 'p') Gloss.Down _ _) world = do
  return $ world {worldAutoRestart = not (worldAutoRestart world)}
handleEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyEnter) Gloss.Down _ _) world = do
  grid' <- Random.evalRandIO $ Grid.genNextTile (worldAssets world) (worldGrid world) False
  return $ world {worldGrid = grid'}
handleEvent _ grid = return grid

updateWorld :: Float -> World -> IO World
updateWorld _ world =
  if worldContinuousGen world || worldAutoRestart world
    then do
      grid' <- Random.evalRandIO $ Grid.genNextTile (worldAssets world) (worldGrid world) (worldAutoRestart world)
      return $ world {worldGrid = grid'}
    else return world

renderWorld :: World -> IO Gloss.Picture
renderWorld world = Grid.drawGrid (worldAssets world) (worldGrid world)
