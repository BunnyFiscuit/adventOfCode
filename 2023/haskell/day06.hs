module Day6 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "6"
  --putStrLn $ show contents
  let xs = map (words . drop 1 . dropWhile (/=':')) contents
  print xs
  let races = initRaces xs
  print races
  let p1 = part1 races
  print p1
  let race = head $ initRaces $ map (\x -> [concat x]) xs
  print race
  print $ length $ filter (>(d race)) [n * ((t race)-n) | n <- [0..(t race)]]

type Time = Int
type Distance = Int
data Race = Race { t :: Int, d :: Int } deriving (Show)

initRaces :: [[String]] -> [Race]
initRaces [[],[]] = []
initRaces [(x:xs), (y:ys)] = Race (read x) (read y) : initRaces [xs, ys]

part1 :: [Race] -> Int
part1 [] = 1
part1 (r:rs) = sim r * part1 rs

sim :: Race -> Int
sim (Race t d) = length $ [1 | n <- [0..t], n * (t-n) > d]

-- sim :: Race -> Int
-- sim race = sim' race 0
--   where sim' :: Race -> Int -> Int
--         sim' r@(Race t d) n
--           | t == n = 0
--           | otherwise = if n * (t-n) > d then 1 + (sim' r (n+1)) else sim' r (n+1)
