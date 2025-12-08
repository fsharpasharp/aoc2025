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
unionFind = fmap concat . mapM go
  where
    go (a, b, _) = do
      dMap <- get
      let x = findParent a dMap
          y = findParent b dMap
      case (x, y) of
        (Nothing, Nothing) -> do
          let root = min a b
          modify (M.insert a root)
          modify (M.insert b root)
          pure [(a, b)]
        (Just aIndex, Just bIndex) -> do
          if aIndex == bIndex
            then pure []
            else do
              modify (M.insert bIndex aIndex)
              pure [(a, b)]
        (Just aIndex, _) -> do
          modify (M.insert b aIndex)
          pure [(a, b)]
        (_, Just bIndex) -> do
          modify (M.insert a bIndex)
          pure [(a, b)]

-- Consider path compression.
findParent :: (Ord t) => t -> Map t t -> Maybe t
findParent z m =
  case M.lookup z m of
    Nothing -> Nothing
    Just next
      | next == z -> Just z
      | otherwise -> findParent next m

day08 :: IO (Int, Integer)
day08 = do
  ps <- parseOrDie points <$> readFile "data/day08.in"
  let idxPoints = zipWith (\x y -> IdxPoint {index = x, point = y}) [0 ..] ps
      ds = distances idxPoints
      partial = execState (unionFind (take 1000 ds)) M.empty

  let mostFrequent = reverse . sort . fmap length . group . sort
      a = product . take 3 . mostFrequent . fmap ((`findParent` partial) . fst) . M.toList $ partial

  let full = evalState (unionFind ds) M.empty
  let lastConnected = last full
      indices = [\findee -> find (\x -> index x == findee) idxPoints] <*> [fst lastConnected, snd lastConnected]
      xCoord = fmap ((\(Point z) -> head z) . point) . catMaybes
      b = product . xCoord $ indices

  return (a, b)
