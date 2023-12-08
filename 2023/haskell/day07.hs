module Day7 where
import FileReader
import Data.List
import Data.Char

main :: IO ()
main = do
  contents <- readF' "test"
  print contents

-- 1. High card ✅
-- 2. One pair ✅
-- 3. Two pairs ✅
-- 4. Three of a kind ✅
-- 5. Full house ✅
-- 6. Four of a kind ✅
-- 7. Five of a kind ✅
rank :: String -> Int
rank xs = rank' (group (sort xs))
  where rank' :: [String] -> Int
        rank' cs
          | length cs == 1 = 7 -- five of a kind
          | length cs == 2 && fourKind cs = 6 -- four of a kind
          | length cs == 2 && fullHouse cs = 5 -- full house
          | length cs == 3 && not (twoPair cs) = 4 -- three of a kind
          | length cs == 3 = 3 -- two pair
          | length cs == 4 = 2 -- one pair
          | otherwise = 1 -- high card

-- assume sorted input
twoPair :: [String] -> Bool
twoPair xs = length (filter (\x -> length x == 2) xs) == 2

-- assume sorted input
fullHouse :: [String] -> Bool
fullHouse xs = elem 3 xs' && elem 2 xs'
  where xs' = map length xs

-- assume sorted input
fourKind :: [String] -> Bool
fourKind xs = 4 `elem` xs'
  where xs' = map length xs

compareHand :: String -> String -> Ordering
compareHand xs ys
  | rank xs > rank ys = GT
  | rank xs < rank ys = LT
  | otherwise = compareHand' xs ys

compareHand' :: String -> String -> Ordering
compareHand' [] [] = EQ
compareHand' [] _ = LT
compareHand' _ [] = GT
compareHand' (x:xs) (y:ys)
  | x > y = LT
  | x < y = GT
  | otherwise = compareHand' xs ys

