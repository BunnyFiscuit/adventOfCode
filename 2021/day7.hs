module Day7 where
import System.IO
import Data.Char

main :: IO ()
main = do
  contents <- readFile "inputs/7.txt"
  let ns = reader contents
  let m  = map (alignCosts ns) [(minimum ns)..(maximum ns)]
  let sums  = map sum m
  putStrLn $ show (minimum sums)
  
alignCosts :: [Int] -> Int -> [Int]
alignCosts [] _ = []
alignCosts (x:xs) a = sum [1..k] : alignCosts xs a
  where k = abs (x-a)

reader :: String -> [Int]
reader [] = []
reader xs = n : reader xs'
  where xs' = drop 1 $ dropWhile (/=',') xs
        n   = read (takeWhile (/=',') xs) :: Int
