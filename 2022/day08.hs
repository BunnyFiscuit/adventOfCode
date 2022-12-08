-- Change Module Name
module Day8 where
import FileReader
import Data.List
import Data.Char

ex :: [[Int]]
ex = [[3,0,3,7,3],[2,5,5,1,2],[6,5,3,3,2],[3,3,5,4,9],[3,5,3,9,0]]

main :: IO ()
main = do
  contents <- readF' "8"
  let grid  = map (map digitToInt) contents
  let l     = length grid
  let count = [check (x,y) grid | x <- [0..l-1], y <- [0..l-1]] :: [Int]
  let sm    = sum count :: Int
  putStrLn $ "part 1: " ++ show sm
  let sc    = [scenic (x,y) grid | x <- [0..l-1], y <- [0..l-1]]
  putStrLn $ "part 2: " ++ show (maximum sc)

-- ^ Part 1
check :: (Int,Int) -> [[Int]] -> Int
check _ [] = 0
check p@(x,y) rs
  | y == 0 || y == rl-1  = 1
  | x == 0 || x == rl-1  = 1
  | otherwise            = check' p rs
  where rl = length (head rs)

check' :: (Int, Int) -> [[Int]] -> Int
check' (x,y) [] = 0
check' (x,y) rs
  | up || down || left || right = 1
  | otherwise                        = 0
  where up = and (vAbove e (x,y-1) rs)
        down = and (vBelow e (x, y+1) rs)
        left = and (vLeft e (x-1, y) rs)
        right = and (vRight e (x+1,y) rs)
        e = get (x,y) rs
        ll = length rs

-- ^ Part 2
scenic :: (Int, Int) -> [[Int]] -> Int
scenic p [] = 0
scenic p@(x,y) rs
  | y == 0 || y == rl-1  = 1
  | x == 0 || x == rl-1  = 1
  | otherwise            = scenic' p rs
  where rl = length (head rs)

scenic' :: (Int, Int) -> [[Int]] -> Int
scenic' (x,y) [] = 0
scenic' (x,y) rs = up * left * right * down
  where e     = get (x,y) rs
        up    = see (vAbove e (x, y-1) rs)
        down  = see (vBelow e (x, y+1) rs)
        left  = see (vLeft  e (x-1, y) rs)
        right = see (vRight e (x+1, y) rs)

see :: [Bool] -> Int
see [] = 0
see (False:rs) = 1
see (True:rs)  = 1 + see rs

get :: (Int, Int) -> [[Int]] -> Int
get (x,y) rs = row !! max 0 x
  where row = head $ take 1 (drop y rs)

vAbove :: Int -> (Int, Int) -> [[Int]] -> [Bool]
vAbove e (x,y) rs
  | y > 0    = (e > vs) : vAbove e (x,y-1) rs
  | otherwise = [e > vs]
  where vs = get (x,y) rs

vBelow :: Int -> (Int, Int) -> [[Int]] -> [Bool]
vBelow e (x,y) rs
  | y < length rs - 1 = (e > vs) : vBelow e (x,y+1) rs
  | otherwise         = [e > vs]
  where vs = get (x,y) rs

vRight :: Int -> (Int, Int) -> [[Int]] -> [Bool]
vRight e (x,y) rs
  | x < length rs - 1 = (e > vs) : vRight e (x+1,y) rs
  | otherwise         = [e > vs]
  where vs = get (x,y) rs

vLeft :: Int -> (Int, Int) -> [[Int]] -> [Bool]
vLeft e (x,y) rs
  | x > 0 = (e > vs) : vLeft e (x-1,y) rs
  | otherwise         = [e > vs]
  where vs = get (x,y) rs
