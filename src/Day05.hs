module Day05 (day05) where

import Data.List (foldl', sortOn)
import Data.Text.IO (readFile)
import Solution (Parser, integer, parseOrDie)
import Text.Megaparsec (optional, some)
import Text.Megaparsec.Char (char, newline, space)
import Prelude hiding (readFile)

data Range = Range Integer Integer deriving (Show)

ranges :: Parser [Range]
ranges = some $ do
  lower <- integer
  _ <- char '-'
  upper <- integer
  _ <- newline
  return (Range lower upper)

fromContents :: Parser ([Range], [Integer])
fromContents = do
  rs <- ranges
  _ <- space
  numbers <- some (integer <* optional newline)
  return (rs, numbers)

mergeRanges :: [Range] -> [Range]
mergeRanges =
  foldl' step [] . sortOn lower
  where
    lower (Range l _) = l
    step [] r = [r]
    step (x@(Range a b) : xs) y@(Range c d)
      | b < c = y : x : xs
      | otherwise = Range a (max b d) : xs

day05 :: IO (Int, Integer)
day05 = do
  (rs, ns) <- parseOrDie fromContents <$> readFile "data/day05.in"

  let betweenRange a (Range l u) = l <= a && a <= u
  let a = length . filter (\x -> any (betweenRange x) rs) $ ns

  let distance (Range l u) = u - l + 1
  let b = sum . fmap distance . mergeRanges $ rs

  return (a, b)
