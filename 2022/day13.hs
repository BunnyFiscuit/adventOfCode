{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day13 where
import FileReader
import Data.List
import Text.Read

main :: IO ()
main = do
  contents <- readF' "13"
  let gs = group' contents
  print gs
  rs <- run gs 1 []
  print rs
  print (sum rs)

run :: [(String, String)] -> Int -> [Int] -> IO [Int]
run [] _ n = return n
run ((x,y):xs) i n = do
  ro <- rightOrder x y
  let n' = if ro then i : n else n
  run xs (i+1) n'

rightOrder :: String -> String -> IO Bool
rightOrder [] _ = return True
rightOrder _ [] = return False
rightOrder "[]" _ = return True
rightOrder _ "[]" = return False
rightOrder ('[':x:xs) ('[':y:ys)
  | x == '[' && y == '[' = do
    let ro = rightOrder' (x : takeWhile (/=']') xs ++ "]") (y : takeWhile (/=']') ys ++ "]")
    return ro
  | x == '[' && y /= '[' = do
    let ro = rightOrder' (x : takeWhile (/=']') xs ++ "]") ['[', y, ']']
    return ro
  | x /= '[' && y == '[' = do
    let ro = rightOrder' ['[', x, ']'] (y : takeWhile (/=']') ys ++ "]")
    return ro
  | x /= '[' && y /= '[' = do
    let ro = (a < b) || (a == b)
    ro' <- rightOrder xs ys
    return (ro && ro')
  where a = read [x] :: Int
        b = read [y] :: Int

rightOrder (x:xs) (y:ys)
  | x == ',' && y == ',' = rightOrder xs ys
  | x == ',' && y == ']' = return False -- left side has more, out of order
  | x == ']' && y == ',' = return True -- right side has more, correct order
  | x' < y'   = return True
  | x' > y'   = return False
  | otherwise = rightOrder xs ys
  where x' = read [x] :: Int
        y' = read [y] :: Int

rightOrder' :: String -> String -> Bool
rightOrder' xs ys
  | int xs && int ys   = (read xs :: Int)   <= (read ys :: Int)
  | list xs && list ys = (read xs :: [Int]) <= (read ys :: [Int])
rightOrder' _ _        = False

readIt :: String -> Either Int [Int]
readIt x = case readMaybe x :: Maybe [Int] of
            Just a  -> Right a
            Nothing -> Left (read x :: Int)

comp :: [Int] -> [Int] -> Ordering
comp [] [] = EQ
comp [] _  = LT
comp _ []  = GT
comp (x:xs) (y:ys)
  | x == y    = comp xs ys
  | x <  y    = LT
  | otherwise = GT

int :: String -> Bool
int xs = case readMaybe xs :: Maybe Int of
  Just x  -> True
  Nothing -> False

list :: String -> Bool
list xs = case readMaybe xs :: Maybe [Int] of
  Just x  -> True
  Nothing -> False

group' :: [String] -> [(String, String)]
group' [] = []
group' xs = (x,y) : group' (drop 3 xs)
  where x = head $ take 1 xs
        y = head $ take 1 (drop 1 xs)