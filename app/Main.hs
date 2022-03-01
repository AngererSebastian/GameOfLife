module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Lib
import StepSimulation

cellHeight = 20

backgroundColor = makeColor 0 (0x2b / 255) (0x36 / 255) 1

forgroundColor = makeColor (0x85 / 255) (0x99 / 255) 0 1

window = InWindow "Game of Life" (400, 400) (100, 100)

drawAliveCell = rectangleSolid cellHeight cellHeight

cellsPic :: Alives -> Picture
cellsPic aliveCells = pictures $ map draw alives
  where
    off i = cellHeight * i
    draw (i, j) = translate (off i) (off j) drawAliveCell
    alives = map (bimap fromInteger fromIntegral) aliveCells

windowPic = color forgroundColor . cellsPic

main :: IO ()
main = simulateBoard $ glider 0 0

presentBoard board =
  display window backgroundColor $ windowPic board

simulateBoard board =
  simulate
    window
    backgroundColor
    3
    board
    windowPic
    (const . const gameOfLife)

glider n m =
  [ (n, m),
    (n + 1, m),
    (n + 2, m),
    (n + 2, m + 1),
    (n + 1, m + 2)
  ]