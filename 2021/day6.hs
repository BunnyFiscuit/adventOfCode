module Day6 where
import System.IO
import Data.Char
import Data.List
import Debug.Trace
main :: IO ()
main = do
  contents <- readFile "inputs/6.txt"
  let ls = lines contents
  let ist = map read [[x] | x <- contents, isDigit x] :: [Int]
  let g = group (sort ist)
  let ls = [0] ++ map length g
  let initState = ls ++ replicate (9 - length ls) 0
  putStrLn $ "Initial State: " ++ show initState
  let count = run 256 initState
  putStrLn $ show count

tick :: [Int] -> [Int]
tick [birth, zero, one, two, three, four, five, six, seven] =
  [zero, one, two, three, four, five, (birth + six), seven, birth]

run :: Int -> [Int] -> Int
run 0 xs = sum xs
run d xs = (run (d-1) t)
  where t = tick xs