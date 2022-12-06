module Day6 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "6"
  let rs  = run (head contents) 4 0
  let rs' = run (head contents) 14 0
  putStrLn $ "part 1: " ++ show rs
  putStrLn $ "part 2: " ++ show rs'

run :: String -> Int -> Int -> Int
run []       i n  = n
run s@(x:xs) i n
  | check s i     = n+i
  | otherwise     = run xs i (n+1)

check :: String -> Int -> Bool
check xs i = length (nub (take i xs)) == i