module Main where

import Draw

main :: IO ()
main = simulateBoard $ glider 0 0

glider n m =
  [ (n, m),
    (n + 1, m),
    (n + 2, m),
    (n + 2, m + 1),
    (n + 1, m + 2)
  ]