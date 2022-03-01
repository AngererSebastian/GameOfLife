module Lib where --(bar, gameOfLife, Alives) where

import Data.Function (fix)
import Data.Ix (inRange)
import Data.List (intersect, nub)
import GHC.Real (Integral)

type Point = (Integer, Integer)

type Alives = [Point]

neighbors (n, m) = [(n + x, m + y) | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]

bar =
  [ (1, 2),
    (2, 2),
    (3, 2)
  ]

countAlive board = length . intersect board . neighbors

survivors board = filterByNeighbors (inRange (2, 3)) board board

filterByNeighbors f board = filter (f . countAlive board)

potentialBirths :: [Point] -> [Point]
potentialBirths board = filter (`notElem` board) . nub . concatMap neighbors $ board

births board = filterByNeighbors (== 3) board $ potentialBirths board

gameOfLife :: Alives -> Alives
gameOfLife board = survivors board ++ births board