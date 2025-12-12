{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 (day10) where

import Data.Bits
import Data.Functor
import Data.List (subsequences)
import Data.Text.IO (readFile)
import Solution (Parser, integer, parseOrDie)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (readFile)


parseDelim :: (Char, Char) -> Parser a -> Parser [a]
parseDelim (startDelim, endDelim) inside = do
  _ <- char startDelim
  ls <- some inside
  _ <- char endDelim
  return ls

parseLights :: Parser [Bool]
parseLights = parseDelim ('[', ']') (False <$ char '.' <|> True <$ char '#')

parseButtons :: Parser [Integer]
parseButtons = parseDelim ('(', ')') parseInteger

parseJoltage :: Parser [Integer]
parseJoltage = parseDelim ('{', '}') parseInteger

parseInteger :: Parser Integer
parseInteger = integer <* optional (try . char $ ',')

data Row = Row
  { lights :: [Bool],
    buttons :: [[Integer]],
    joltage :: [Integer]
  }
  deriving (Show)

parseAll :: Parser [Row]
parseAll = some $ do
  ls <- parseLights
  space
  bs <- some (parseButtons <* space)
  js <- parseJoltage
  void newline <|> eof
  pure (Row ls bs js)

class ToBit a where
  toBit :: a -> Int

instance ToBit [Bool] where
  toBit xs =
    foldl
      (\acc (i, b) -> if b then setBit acc i else acc)
      zeroBits
      (zip [0 ..] xs)

instance ToBit [Integer] where
  toBit = foldl1 (.|.) . fmap (bit . fromInteger)

findLights :: Row -> Int
findLights (Row t bs _) = combos
  where
    target = toBit t
    buttonCombos = subsequences . fmap toBit $ bs
    combos = minimum . fmap length . filter ((== target) . foldl xor 0) $ buttonCombos

day10 :: IO Int
day10 = do
  rows <- parseOrDie parseAll <$> readFile "data/day10.in"
  pure (sum . fmap findLights $ rows)