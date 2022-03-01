module Main where

import Draw
import ParsePlain
import System.Environment

main :: IO ()
main = getArgs >>= (readFile . head) >>= (simulateBoard . parsePlain)