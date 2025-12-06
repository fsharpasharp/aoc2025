module Day01 (day01) where

data Rotation = L Int | R Int deriving Show

parseRotation :: String -> Rotation
parseRotation (x:xs) = case x of
    'L' -> L (read xs)
    'R' -> R (read xs)
    
dial :: Int
dial = 100

rotate :: Int -> Rotation -> Int
rotate a (L b) = (a-b) `mod` dial
rotate a (R b) = (a+b) `mod` dial

passedZeros :: Int -> Rotation -> Int
passedZeros 0 (L b) = b `div` dial
passedZeros a (L b)  
    | a > b = 0
    | otherwise = 1 + (b-a) `div` dial
passedZeros a (R b) = (a+b) `div` dial

day01 :: IO (Int, Int)
day01 = do
    rotations <- fmap parseRotation . lines <$> readFile "data/day01.in"
    let states = scanl rotate 50 rotations
    return (length . filter (==0) $ states, sum . zipWith passedZeros states $ rotations)