module Day02 where

import Data.Text.IO (readFile)
import Solution
import Text.Megaparsec (many, optional, some)
import Text.Megaparsec.Char (char, spaceChar)
import Prelude hiding (readFile)

data Range = Range Integer Integer deriving (Show)

ranges :: Parser [Range]
ranges = some $ do
  lower <- integer
  _ <- char '-'
  upper <- integer
  _ <- optional . char $ ','
  _ <- many spaceChar
  return (Range lower upper)

repeatingOnce :: Integer -> Bool
repeatingOnce x = a == b
  where
    s = show x
    (a, b) = splitAt (length s `div` 2) s

repeating :: (Show p) => p -> Bool
repeating x = any isPeriodic [1 .. len `div` 2]
  where
    s = show x
    len = length s
    isPeriodic n = len `mod` n == 0 && take len repeated == s
      where
        repeated = cycle (take n s)

numsInRange :: Range -> [Integer]
numsInRange (Range lower upper) = [lower .. upper]

day02 = do
  rs <- parseOrDie ranges <$> readFile "data/day02.in"
  return [sum . concatMap (filter f . numsInRange) $ rs | f <- [repeatingOnce, repeating]]