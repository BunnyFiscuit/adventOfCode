module Day9 where
import FileReader
import Data.List
import Data.Char
import Debug.Trace

main :: IO ()
main = do
  ls <- readF 9
  -- print ls
  gs <- runFirst ls
  runSecond ls gs
  
type Pos = (Int,Int)

runFirst :: [String] -> IO [Pos]
runFirst ls = do
  let end = (length (head ls), length ls)
  let points = genPoints end
  let res = map (\p -> lowest (getValue ls p) (adjVals ls p)) points
  let ps = zip points res
  let zs = zip res (concat ls)
  let filMap = map (snd) (filter fst zs)
  putStrLn $ "Part 1: " ++ show (calcRiskLevel filMap)
  return $ map fst (filter snd ps)
  
runSecond :: [String] -> [Pos] -> IO ()
runSecond xs ps = do
  let m = map (\x -> [x]) ps
  let ms = map length (map (basin xs) m)
  putStrLn $ "Part 2: " ++ show (calcPart2 ms)

calcPart2 :: [Int] -> Int
calcPart2 = (product . take 3 . reverse . sort)

basin :: [String] -> [Pos] -> [Pos]
basin xs ps
  | sps == srs = ps
  | otherwise = basin xs rd
    where rs  = concat $ map (adjVals' xs) ps
          sps = sort ps 
          srs = sort rd
          rd  = rmdups (ps ++ rs)

lowest :: Int -> [Int] -> Bool
lowest n ns = minimum ns > n

getValue :: [String] -> Pos -> Int
getValue xs (x,y) = digitToInt $ (xs !! y) !! x

adjPos :: Pos -> [Pos]
adjPos (x,y) = [(x-1,y),(x,y-1),(x,y+1),(x+1,y)]

adjVals :: [String] -> Pos -> [Int]
adjVals xs p = map (getValue xs) ajs
  where end = (length (head xs), length xs)
        ajs = filterInvalid end (adjPos p)

adjVals' :: [String] -> Pos -> [Pos]
adjVals' xs p = ajs
  where end = (length (head xs), length xs)
        ajs = filter9 xs (filterInvalid end (adjPos p))

filterInvalid :: Pos -> [Pos] -> [Pos]
filterInvalid _    []     = []
filterInvalid end@(endX,endY) ((x,y):ps)
  | x < endX && x >= 0 && y < endY && y >= 0 = (x,y) : filterInvalid end ps
  | otherwise            = filterInvalid end ps
  
genPoints :: Pos -> [Pos]
genPoints (x,y) = [(a,b) | b <- [0..y-1], a <- [0..x-1]]

calcRiskLevel :: String -> Int
calcRiskLevel [] = 0
calcRiskLevel (x:xs) = 1 + digitToInt x + calcRiskLevel xs

filter9 :: [String] -> [Pos] -> [Pos]
filter9 xs ps = filter (\p -> getValue xs p /= 9) ps 

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
