{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day11 (day11) where

import Control.Monad (zipWithM)
import Control.Monad.State.Strict 
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text.IO (readFile)
import Solution
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, hspace, letterChar, newline)
import Prelude hiding (readFile)

type Connection = Map String [String]

parseConnections :: Parser Connection
parseConnections = M.unions <$> go `sepEndBy` newline
  where
    word = some letterChar
    go = do
      w <- word
      _ <- char ':'
      _ <- hspace
      ws <- word `sepEndBy` hspace
      pure (M.singleton w ws)

type Cache = Map (String, String) Integer

dfs :: Connection -> String -> String -> State Cache Integer
dfs m w t
  | w == t = pure 1
  | w == "out" = pure 0
  | otherwise = memo (w,t) (sum <$> traverse (\c -> dfs m c t) neighbors)
  where
    neighbors = case M.lookup w m of
      Nothing -> error ("w" ++ show w)
      Just xs -> xs
    memo key compute = do
        cache <- get
        case M.lookup key cache of
            Just v  -> pure v
            Nothing -> do
                v <- compute
                modify (M.insert key v)
                pure v

runMemo :: Map String [String] -> State Cache (Integer, Integer)
runMemo m = do
  a <- paths ["you", "out"]
  b <- paths ["svr", "fft", "dac", "out"]
  c <- paths ["svr", "dac", "fft", "out"]
  pure (a, b + c)
  where
    paths (x:xs) = product <$> zipWithM (dfs m) (x:xs) xs

day11 :: IO (Integer, Integer)
day11 = do
  m <- parseOrDie parseConnections <$> readFile "data/day11.in"
  pure (evalState (runMemo m) M.empty)
