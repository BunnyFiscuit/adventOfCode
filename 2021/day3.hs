module Day3 where

import System.IO
import Data.Char
import Data.List
import Debug.Trace


main :: IO ()
main = do
  content <- readFile "inputs/3.txt"
  let ls = lines content
  let t = transpose ls
  let m = map count t
  let g = map common m
  let e = inverse g
  -- putStrLn $ "g: " ++ show g ++ ", e: " ++ show e
  let (gi, ei) = (binToDec (read g :: Int), binToDec (read e :: Int))
  -- putStrLn $ "task 3.1: " ++ show (gi * ei)
  ox <- run 0 '1' ls
  sc <- run 0 '0' ls
  putStrLn $ show (ox, sc)
  putStrLn $ show (binToDec (read ox :: Int) * binToDec (read sc :: Int))
  
common :: (Int,Int) -> Char
common (o,z) = if o > z then '1' else '0'

inverse :: String -> String
inverse [] = []
inverse (c:ch) = i : inverse ch
  where i = if c == '1' then '0' else '1'

count :: String -> (Int, Int)
count str = (count' '1', count' '0')
  where count' bit = length (filter (==bit) str)

binToDec :: Int -> Int
binToDec 0 = 0
binToDec n = 2 * binToDec (div n 10) + (mod n 10)

posEqual :: [String] -> Int -> Bool
posEqual str n = sum (diff str n) == 0
  where 
    diff :: [String] -> Int -> [Int]
    diff [] _ = []
    diff (s:ss) p 
      | s !! p == '0' = -1 : diff ss p
      | otherwise = 1 : diff ss p 

posEqPick :: [String] -> Int -> Char -> [String]
posEqPick str n ch
  | posEqual str n = filter (\x -> x !! n == ch) str
  | otherwise = str

ff :: Int -> Char -> [String] -> [String]
ff _ _ [] = []
ff i bit str = filter (\x -> x !! i == bit) str

createBitArr :: String -> [(Int, String)]
createBitArr str = tail (map (\x -> (x-1, take x str)) [0..(length str)])

indexOf :: Char -> String -> Int
indexOf _  []     = -1
indexOf ch (x:xs) = if ch == x then 0 else 1 + indexOf ch xs


run :: Int -> Char -> [String] -> IO String
run _   _ []  = return $ ""
run pos _ [x] = return $ x
run pos ch arr = do
  -- putStrLn $ "\narr: " ++ show arr
  case posEqual arr pos of
    True -> do 
      let peq = posEqPick arr pos ch
      -- putStrLn $ "peq: " ++ show peq
      run pos ch peq
    False -> do
      let b = map common (map count (transpose arr))
      let bits = if ch == '1' then b else inverse b
      -- putStrLn $ "bits: " ++ show bits
      -- putStrLn $ "bits !! " ++ show pos ++ ": " ++ show (bits !! pos)
      let f = ff pos (bits !! pos) arr
      run (pos+1) ch f
  