module Day2 where

import FileReader
import Data.List

data Play = P Int Int deriving (Show, Eq)

main :: IO ()
main = do
  contents <- readF 2
  let strat1 = sum $ map playScore (map toPlay contents)
  let strat2 = sum $ map playScore (map toPlay' contents)
  putStrLn $ "part 1: " ++ show strat1
  putStrLn $ "part 2: " ++ show strat2

rps :: Char -> Int
rps c
  | c == 'A' || c == 'X' = 0 -- Rock
  | c == 'B' || c == 'Y' = 1 -- Paper
  | c == 'C' || c == 'Z' = 2 -- Scissors

playScore :: Play -> Int
playScore p@(P _ you) = outcomeScore p + (you + 1) 

outcomeScore :: Play -> Int
outcomeScore (P elf you)
  | (elf + 1) `mod` 3 == you = 6 -- you win!
  | elf == you               = 3 -- draw
  | otherwise                = 0 -- loss

toPlay :: String -> Play
toPlay (elf : ' ' : you : []) = P (rps elf) (rps you)

toPlay' :: String -> Play
toPlay' (elf : ' ' : you : []) = P (rps elf) toWin
  where oc :: Char -> Int
        oc 'X' = 0
        oc 'Y' = 3
        oc 'Z' = 6
        toWin = winningHand (rps elf) (oc you)

-- elf hand -> needed outcome -> needed hand
winningHand :: Int -> Int -> Int
winningHand elf 0       -- loss
  | elf  == 0 = 2
  | elf  == 1 = 0
  | elf  == 2 = 1
winningHand elf 6       -- win
  | elf == 0 = 1
  | elf == 1 = 2
  | elf == 2 = 0
winningHand elf 3 = elf -- draw