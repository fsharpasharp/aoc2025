module Day04 (day04) where

import Data.Array.IArray
  ( Array,
    IArray (bounds),
    Ix (range),
    listArray,
    (!?),
    (//),
  )

type Grid a = Array (Int, Int) a

fromLines :: [String] -> Grid Char
fromLines rows =
  let h = length rows
      w = length (head rows)
      b = ((0, 0), (h - 1, w - 1))
   in listArray b (concat rows)

adjacent :: (Eq a, Num a) => (a, a) -> [(a, a)]
adjacent (y, x) =
  [(y + i, x + j) | i <- ds, j <- ds, (i, j) /= (0, 0)]
  where
    ds = [-1, 0, 1]

rolls :: Grid Char -> [((Int, Int), Char)]
rolls g =
  [ (p, '.')
    | p <- range (bounds g),
      g !? p == Just '@',
      length [() | q <- adjacent p, g !? q == Just '@'] < 4
  ]

fixRoll :: Grid Char -> Int
fixRoll g = go g 0
  where
    go grid acc =
      case rolls grid of
        [] -> acc
        update -> go (grid // update) (acc + length update)

day04 :: IO (Int, Int)
day04 = do
  g <- fromLines . lines <$> readFile "data/day04.in"
  pure (length . rolls $ g, fixRoll g)