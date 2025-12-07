module Day01 (day01) where

data Rotation = L Int | R Int deriving (Show)

parseRotation :: String -> Rotation
parseRotation ('L' : xs) = L (read xs)
parseRotation ('R' : xs) = R (read xs)
parseRotation _ = error "Unexpected string"

dial :: Int
dial = 100

rotate :: Int -> Rotation -> Int
rotate a b = (`mod` dial) . (a +) $ case b of
      L c -> -c
      R c -> c

passedZeros :: Int -> Rotation -> Int
passedZeros 0 (L b) = b `div` dial
passedZeros a (L b)
  | a > b = 0
  | otherwise = 1 + (b - a) `div` dial
passedZeros a (R b) = (a + b) `div` dial

day01 :: IO (Int, Int)
day01 = do
  rotations <- fmap parseRotation . lines <$> readFile "data/day01.in"
  let states = scanl rotate 50 rotations

  pure
    ( length . filter (== 0) $ states,
      sum . zipWith passedZeros states $ rotations
    )