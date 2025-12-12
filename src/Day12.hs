{-# LANGUAGE OverloadedStrings #-}

module Day12 (day12) where

import Data.Functor
import Data.Text.IO (readFile)
import Solution (Parser, integer, parseOrDie)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (readFile)

parseShape :: Parser Integer
parseShape = do
    _ <- integer
    _ <- char ':'
    _ <- newline
    bs <- some symbol `sepEndBy` newline
    pure (sum . fmap sum $ bs)
    where symbol = 0 <$ char '.' <|> 1 <$ char '#'

parseGoal :: Parser (Integer, [Integer])
parseGoal = do
  a <- integer
  _ <- char 'x'
  b <- integer
  _ <- string ": "
  req <- integer `sepEndBy` hspace
  pure (a*b, req)

parseAll :: Parser ([Integer], [(Integer, [Integer])])
parseAll = do
  ss <- some (try (parseShape <* newline))
  g <- parseGoal `sepEndBy` newline
  pure (ss, g)

reject :: (Ord c, Num c) => [c] -> (c, [c]) -> Bool
reject shapes (area, counts) =
  area < sum (zipWith (*) shapes counts)

day12 :: IO Int
day12 = do
  (shapes, goals) <- parseOrDie parseAll <$> readFile "data/day12.in"
  return (length . filter (not . reject shapes) $ goals)