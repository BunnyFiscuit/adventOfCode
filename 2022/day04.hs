module Day4 where
import FileReader
import Data.List

main = do
  contents <- readF 4
  let paired = pair contents
  let f = length . filter (==True)
  putStrLn $ "part 1: " ++ show (f (map contains paired))
  putStrLn $ "part 2: " ++ show (f (map overlaps paired))

data Pair = P (Int,Int) (Int,Int) deriving (Show, Eq)

-- ["3-5,2-4",..] -> [P (3,5) (2,4),..]
pair :: [String] -> [Pair]
pair [] = []
pair (x:xs) = P (read l1, abs (read h1)) (read l2, abs (read h2)) : pair xs
  where (l1, h1) = break (=='-') f
        (l2, h2) = break (=='-') (drop 1 s)
        (f,  s) = break (==',') x

contains :: Pair -> Bool
contains (P (x0,y0) (x1,y1)) 
  = x0 >= x1 && y0 <= y1 || x1 >= x0 && y1 <= y0

overlaps :: Pair -> Bool
overlaps (P (x0, y0) (x1, y1))
  = length (intersect [x0..y0] [x1..y1]) > 0