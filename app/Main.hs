module Main where

import Data.Maybe
import Draw
import ParsePlain
import System.Environment

headMaybe (x : xs) = Just x
headMaybe [] = Nothing

usage = putStrLn "usage:\n\tgameOfLife <cellsFile>"

main :: IO ()
--main = getArgs >>= (readFile . head) >>= (simulateBoard . parsePlain)
main = do
  args <- getArgs
  let file = headMaybe args

  case file of
    Just f -> readFile f >>= (simulateBoard . parsePlain)
    Nothing -> usage