module StepSimulation (stepSimulation) where

import Data.Functor (($>))
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game

stepSimulation ::
  Display ->
  Color ->
  Int ->
  Bool ->
  model ->
  (model -> Picture) ->
  (ViewPort -> Float -> model -> model) ->
  IO ()
stepSimulation display color fps isPaused initial draw update = playIO display color fps init (return . worldDraw) (handleEvent update) (\a b -> return $ worldUpdate a b)
  where
    init = World isPaused viewPortInit initial
    worldDraw (World _ _ m) = draw m
    worldUpdate delta (World False view model) = World False view (update view delta model)
    worldUpdate delta (World True view model) = World True view model

data World model = World Bool ViewPort model

handleEvent update (EventKey key _ _ (x, y)) (World paused view m) =
  case key of
    Char ' ' -> putStrLn "toggle pause" $> World (not paused) view m
    SpecialKey KeyEnter | paused -> putStrLn "step" $> World paused view (update view 1 m)
    _ -> return $ World paused view m
handleEvent _ _ w = return w