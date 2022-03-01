module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Lib
import StepSimulation

cellHeight = 20

screenH = 1080

screenW = 1920

rows = screenH / cellHeight

cols = screenW / cellHeight

drawAliveCell =
  let width = cellHeight / 10
      height = (cellHeight - 2) * sqrt 2
      rect = rectangleSolid height width
      negCellHalf = (-cellHeight / 2)
   in translate negCellHalf negCellHalf $
        pictures
          [ rotate 45 rect,
            rotate (-45) rect
          ]

cellsPic :: Alives -> Picture
cellsPic aliveCells = pictures $ map draw alives
  where
    off i = cellHeight * i
    draw (i, j) = translate (off i) (off j) drawAliveCell
    alives = map (bimap fromInteger fromIntegral) aliveCells

--aliveCells = filter (isAlive . uncurry (curry grid `on` floor)) $ (,) <$> [1 .. cols] <*> [1 .. rows]

gridPic = pictures $ vert ++ horiz
  where
    off i = i * cellHeight
    drawLines f n = map (line . f) [1 .. n]
    vertF i = [(off i, 0), (off i, screenH)]
    horizF i = [(0, off i), (screenW, off i)]
    vert = drawLines vertF cols
    horiz = drawLines horizF rows

golPic grid = translate (-1920 / 2) (-1080 / 2) $ pictures [cellsPic grid, gridPic]

main :: IO ()
main = simulateBoard $ glider 20 20

presentBoard board =
  display FullScreen (greyN 0.3) (golPic board)

simulateBoard board =
  simulate
    FullScreen
    (greyN 0.3)
    1
    board
    golPic
    (const . const gameOfLife)

glider n m =
  [ (n, m),
    (n + 1, m),
    (n + 2, m),
    (n + 2, m + 1),
    (n + 1, m + 2)
  ]