module Day03 (day03) where

import Data.Char (digitToInt)

nBatteries :: (Ord a) => Int -> [a] -> [a]
nBatteries = go
  where
    go 0 _ = []
    go remaining ys = best : go (remaining - 1) rest
      where
        available = length ys - remaining + 1
        best = maximum . take available $ ys
        rest = tail . dropWhile (/= best) $ ys

day03 :: IO [Int]
day03 = do
  ns <- fmap (fmap digitToInt) . lines <$> readFile "data/day03.in"
  pure
    [ sum
        . fmap
          ( (read :: String -> Int)
              . concatMap show
              . nBatteries x
          )
        $ ns
      | x <- [2, 12]
    ]