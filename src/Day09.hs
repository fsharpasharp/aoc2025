module Day09 (day09) where

import Control.Monad (void)
import Data.Text.IO (readFile)
import Solution (Parser, integer, parseOrDie)
import Text.Megaparsec (MonadParsec (eof), some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Prelude hiding (readFile)
import Data.List (tails)

type Point = (Integer, Integer)

points :: Parser [Point]
points = some (point <* (void newline <|> eof))
  where
    point = do
      a <- integer
      _ <- char ','
      b <- integer
      pure (a, b)

data Line = Line
  { pos :: Integer,
    mini :: Integer,
    maxi :: Integer
  }
  deriving (Show)

data LineType = Vertical Line | Horizontal Line deriving (Show)

type Rectangle = (Integer, Integer, Integer, Integer)

rectangle :: Point -> Point -> Rectangle
rectangle (x1, y1) (x2, y2) = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)

pairwiseRectangles :: [Point] -> [Rectangle]
pairwiseRectangles xs = [rectangle y z | (y : ys) <- tails xs, z <- ys]

area :: Rectangle -> Integer
area (minX, maxX, minY, maxY) = (maxX - minX + 1) * (maxY - minY + 1)

boundaries :: [Point] -> [LineType]
boundaries [] = []
boundaries (p : ps) = fmap tupleToLine . zip (p : ps) $ (ps ++ [p])
  where
    tupleToLine (p1, p2)
      | x_low == x_high = Vertical (Line x_low y_low y_high)
      | y_low == y_high = Horizontal (Line y_low x_low x_high)
      | otherwise = error "Uh oh!"
      where
        (x_low, x_high, y_low, y_high) = rectangle p1 p2

intersectsRect :: (Foldable t) => t LineType -> Rectangle -> Bool
intersectsRect boundary (minX, maxX, minY, maxY) = any go boundary
  where
    go (Vertical (Line x y1 y2)) = minX < x && x < maxX && y2 > minY && y1 < maxY
    go (Horizontal (Line y x1 x2)) = minY < y && y < maxY && x2 > minX && x1 < maxX

day09 :: IO (Integer, Integer)
day09 = do
  ps <- parseOrDie points <$> readFile "data/day09.in"
  let rectangles = pairwiseRectangles ps
      boundary = boundaries ps
      best = maximum . fmap area

  pure
    ( best rectangles,
      best . filter (not . intersectsRect boundary) $ rectangles
    )