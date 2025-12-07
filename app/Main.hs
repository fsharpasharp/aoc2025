{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Control.Monad (zipWithM_)

data AoCAssertion where
  AoCAssertion
    :: (Eq a, Show a)
    => IO a
    -> a
    -> AoCAssertion

assertions :: [AoCAssertion]
assertions =
  [
     AoCAssertion day01 (1191,6858),
     AoCAssertion day02 [53420042388,69553832684],
     AoCAssertion day03 [16946,168627047606506],
     AoCAssertion day04 (1551,9784),
     AoCAssertion day05 (737,357485433193284),
     AoCAssertion day06 [6757749566978,10603075273949],
     AoCAssertion day07 (1600,8632253783011)
  ]


runAssertion :: Int -> AoCAssertion -> IO Bool
runAssertion x (AoCAssertion io expected) = do
  actual <- io
  let ok = actual == expected
  putStrLn $
    show x ++ if ok
      then " ✅"
      else " ❌ expected " ++ show expected ++ ", got " ++ show actual
  pure ok

main :: IO ()
main = zipWithM_ runAssertion [1..] assertions


-- today = AoCAssertion day01 1
-- main :: IO ()
-- main = runAssertion 0 today