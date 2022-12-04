module Day25 where
import System.IO
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF 25
  --putStrLn $ "Initial State:"
  --mapM_ print contents
  --putStrLn ""
  run 0 contents
  

showIt :: [String] -> IO ()
showIt xs = mapM_ putStrLn xs

run :: Int -> [String] -> IO ()
run n xs = do
  let xs' = tick xs
  case and (map (\(x,y) -> x == y) (zip xs' xs)) of
    True -> do
      putStrLn $ "Final State - " ++ show (n-1)
      --mapM_ print xs'
    False -> run (n+1) xs'


tick :: [String] -> [String]
tick xs = transpose downs
  where lefts = map (tickRow '>') xs
        downs = map (tickRow 'v') (transpose lefts)

tickRow :: Char -> String -> String
tickRow c xs = tickRow' 0 c xs

tickRow' :: Int -> Char -> String -> String
tickRow' i c xs 
  | i >= l = xs
  | c /= curr = tickRow' (i+1) c xs
  | otherwise = case i+1 >= l of
  True -> case first of
    '.' -> tickRow' (i+1) c (replace 0 curr (replace i '.' xs))
    _   -> tickRow' (i+1) c xs
  False -> case next of
    '.' -> tickRow' (i+2) c (replace (i+1) curr (replace i '.' xs))
    _   -> tickRow' (i+1) c xs
  where next  = xs !! (i+1)
        curr  = xs !! i
        first = xs !! 0
        l     = length xs
          
replace :: Int -> Char -> String -> String
replace i c xs = take i xs ++ [c] ++ drop (i+1) xs


test :: [String]
test = [
  "v...>>.vv>",
  ".vv>>.vv..",
  ">>.>v>...v",
  ">>v>>.>.v.",
  "v>v.vv.v..",
  ">.>>..v...",
  ".vv..>.>v.",
  "v.v..>>v.v",
  "....v..v.>"]
