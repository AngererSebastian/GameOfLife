module ParsePlain where

import Data.List (isPrefixOf)
import Lib (Alives, Point)

parsePlain :: String -> Alives
parsePlain = map aux . filter ((== 'O') . snd) . index2d . ignoreHeader . lines
  where
    aux ((x, y), _) = (x, -y)

index = zip [0 ..]

index2d :: [[a]] -> [((Integer, Integer), a)]
index2d = concatMap aux . index
  where
    aux (x, as) = map (tuply x) $ index as
    tuply x (y, a) = ((x, y), a)

ignoreHeader :: [String] -> [String]
ignoreHeader = dropWhile (isPrefixOf "!")