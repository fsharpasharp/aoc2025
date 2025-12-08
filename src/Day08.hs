{-# LANGUAGE FlexibleContexts #-}

module Day08 (day08) where

import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Control.Monad.State
  ( MonadState (get),
    StateT,
    evalState,
    execState,
    modify,
  )
import Data.List (find, group, sort, sortOn, tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text.IO (readFile)
import Solution (Parser, integer, parseOrDie)
import Text.Megaparsec (MonadParsec (eof), some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Prelude hiding (lines, readFile)

newtype Point = Point [Integer] deriving (Show)

data IdxPoint = IdxPoint
  { index :: Integer,
    point :: Point
  }
  deriving (Show)

type DistanceTriple = (Integer, Integer, Integer)

points :: Parser [Point]
points = some (triple <* (void newline <|> eof))
  where
    triple = do
      a <- integer
      _ <- char ','
      b <- integer
      _ <- char ','
      c <- integer
      pure (Point [a, b, c])

distances :: [IdxPoint] -> [DistanceTriple]
distances xs = sortOn (\(_, _, x) -> x) [(index a, index b, distance (point a) (point b)) | (a : ys) <- tails xs, b <- ys]
  where
    distance (Point m) (Point n) = sum . fmap (^ 2) . zipWith (-) m $ n

unionFind :: [(Integer, Integer, c)] -> StateT (Map Integer Integer) Identity [(Integer, Integer)]
unionFind = fmap catMaybes . mapM go
  where
    go (a, b, _) = do
      dMap <- get
      case (findParent a dMap, findParent b dMap) of
        (aIndex, bIndex)
          | aIndex == bIndex -> pure Nothing
          | otherwise -> do
              modify (M.insert bIndex aIndex)
              pure (Just (a, b))

-- Consider path compression.
findParent :: (Ord t) => t -> Map t t -> t
findParent z m = maybe z (`findParent` m) (M.lookup z m)

day08 :: IO (Int, Integer)
day08 = do
  ps <- parseOrDie points <$> readFile "data/day08.in"
  let idxPoints = zipWith IdxPoint [0 ..] ps
      ds = distances idxPoints
      allIndices = fmap index idxPoints
      (front, back) = splitAt 1000 ds
      partial = execState (unionFind front) M.empty
      mostFrequent = reverse . sort . fmap length . group . sort
      a = product . take 3 . mostFrequent . fmap (`findParent` partial) $ allIndices

      full = evalState (unionFind back) partial
      (i1, i2) = last full
      lastIndices  = fmap (\i -> find ((== i) . index) idxPoints) [i1, i2]
      xCoord = fmap (head . (\(Point z) -> z) . point) . catMaybes
      b = product . xCoord $ lastIndices

  return (a, b)
