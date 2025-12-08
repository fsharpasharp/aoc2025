{-# LANGUAGE FlexibleContexts #-}

module Day07 (day07) where

import Control.Monad.State.Strict (State, gets, modify', evalState)
import Data.Array.IArray (Array, assocs, listArray, (!?))
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Grid a = Array (Int, Int) a

fromLines :: [String] -> Grid Char
fromLines rows =
  let h = length rows
      w = length (head rows)
      b = ((0, 0), (h - 1, w - 1))
   in listArray b (concat rows)

start :: Grid Char -> (Int, Int)
start = fst . fromJust . find ((== 'S') . snd) . assocs

laser :: Grid Char -> State (Map (Int, Int) Int) (Int, Int)
laser g = fmap (+ 1) <$> go (start g)
  where
    go p@(y, x) = do
      res <- gets (M.lookup p)
      case res of
        Just a -> pure (0, a)
        Nothing ->
          case g !? p of
            Just '^' -> do
              (a, b) <- go (y, x - 1)
              (c, d) <- go (y, x + 1)
              let val = b + d + 1
              modify' (M.insert p val)
              pure (a + c + 1, val)
            Just _ -> go (y + 1, x)
            Nothing -> pure (0, 0)

day07 :: IO (Int, Int)
day07 = do
  g <- fromLines . lines <$> readFile "data/day07.in"
  pure (evalState (laser g) M.empty)