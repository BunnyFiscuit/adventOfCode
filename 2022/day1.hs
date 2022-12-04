module Day1 where

import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF 1
  let split = map (\x -> map (\y -> read y :: Int) x) (mySplit contents)
  let sums = reverse (sort (map sum split))
  putStrLn $ "part 1: " ++ show (sums !! 0)
  putStrLn $ "part 2: " ++ show (sum (take 3 sums))

mySplit :: [String] -> [[String]]
mySplit [] = []
mySplit xs = takeWhile (\x -> x /= "") xs : mySplit (drop 1 rs)
  where rs = dropWhile (\x -> x /= "") xs