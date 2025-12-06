module Day06 (day06) where

import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Data.Text (lines, pack, unpack)
import Data.Text.IO (readFile)
import Solution (Parser, integer, parseOrDie)
import Text.Megaparsec
  ( MonadParsec (try),
    optional,
    satisfy,
    skipMany,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, hspace, space)
import Prelude hiding (lines, readFile)

nums :: Parser [Integer]
nums = some (space *> integer <* space)

numOrSep :: Parser (Maybe Integer)
numOrSep = do
  hspace
  optional . try $ rest
  where
    skipNonDigit = skipMany . satisfy $ (not . isDigit)
    rest = do
      skipNonDigit
      x <- integer
      skipNonDigit
      return x

data Operation = Sum | Product deriving (Show)

runOperation :: Operation -> [Integer] -> Integer
runOperation Product = product
runOperation Sum = sum

operations :: Parser [Operation]
operations = some $ (oph Sum '+' <|> oph Product '*') <* space
  where
    oph :: Operation -> Char -> Parser Operation
    oph op c = op <$ char c

walk :: NonEmpty [Integer] -> Maybe Integer -> NonEmpty [Integer]
walk (x :| xs) (Just y) = (y : x) :| xs
walk (x :| xs) Nothing = [] :| x : xs

day06 :: IO [Integer]
day06 = do
  text <- lines <$> readFile "data/day06.in"

  let matrix = transpose . fmap (parseOrDie nums) . init $ text
      ops = parseOrDie operations . last $ text
      wrap = reverse . toList . foldl walk ([]:|[])
      verticalNums = wrap . fmap (parseOrDie numOrSep . pack) . transpose . fmap unpack $ text

  return ([sum . zipWith runOperation ops] <*> [matrix, verticalNums])