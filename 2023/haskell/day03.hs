module Day where
import FileReader
import Data.List

type Pos = (Int, Int)
type Matrix = [[Char]]

main :: IO ()
main = do
  contents <- readF' "3"
  --putStrLn $ show $ (length (head contents), length contents)
  let xs = run contents (0,0)
  putStr $ "Part 1: "
  putStrLn $ show $ sum xs
  putStr $ "Part 2: "
  run2 contents
  


run :: Matrix -> Pos -> [Int]
run xs (x,y)
  | x > length (head xs) - 1 = run xs (0, y+1)
  | y > length xs - 1 = []
  | otherwise = case checks of 
                  True -> n : run xs (x + (max 1 (length s)), y)
                  False -> run xs (x + (max 1 (length s)), y)  
  where (s, ps) = gf (x,y) xs
        n = if checks then read s :: Int else 0
        checks = any (==True) (map (\l -> checkSymbols l xs) ps)


matrix = ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]

get :: Matrix  -> Pos -> Char
get m (x, y) = (m !! y) !! x

isNmb :: Matrix -> Pos -> Bool
isNmb m p@(x,y) 
  | x < 0 || y < 0 || x > length (head m) -1 || y > length m - 1 = False
  | otherwise = get m p `elem` ['0'..'9'] 

gf :: Pos -> Matrix -> (String, [Pos])
gf pos m = gf' pos ([], []) m

gf' :: Pos -> (String, [Pos]) -> Matrix -> (String, [Pos])
gf' (x, y) (s, ps) m
  | x > length (head m) - 1 = (s, ps)
  | isNmb m (x, y) = gf' (x+1, y) (s ++ [get m (x, y)], ps ++ [(x, y)]) m
  | otherwise = (s, ps)

checkSymbols :: (Int, Int) -> Matrix -> Bool
checkSymbols (x, y) m = any (==True) ms
  where checks = getChecks m (x,y)
        ms = map (\p -> not (get m p `elem` ("." ++ ['0'..'9']))) checks

getChecks :: Matrix -> Pos -> [Pos]
getChecks m (x,y) = [(x1, y), (x2, y), (x, y1), (x, y2), (x1, y1), (x2, y2), (x1, y2), (x2, y1)]
  where x1 = min (x+1) (length (head m) - 1) 
        y1 = min (y+1) (length m - 1)
        x2 = max (x-1) 0
        y2 = max (y-1) 0

-- Part 2

run2 :: Matrix -> IO ()
run2 xs = do
  let gs = findGears xs
  let as = map (\p -> adjacentNumbers xs p) gs
  let fs = map (\a -> filter (/="") (nub a)) as
  let fs' = map readAndMult (filter (\f -> length f == 2) fs)
  -- putStrLn $ show as
  putStrLn $ show $ sum fs'

readAndMult :: [String] -> Int
readAndMult xs = product (map (\x -> read x :: Int) xs)

findGears :: Matrix -> [Pos]
findGears m = filter (\p -> get m p == '*') [(x,y) | x <- [0..length (head m) - 1], y <- [0..length m - 1]]

getBefore :: Matrix -> Pos -> String
getBefore m (x,y)
  | x < 0 = []
  | isNmb m (x,y) = getBefore m (x-1, y) ++ [get m (x, y)]
  | otherwise = []

getAfter :: Matrix -> Pos -> String
getAfter m (x,y)
  | x > length (head m) - 1 = []
  | isNmb m (x,y) = get m (x, y) : getAfter m (x+1, y)
  | otherwise = []

getNumber :: Matrix -> Pos -> String
getNumber m p@(x,y) = getBefore m (x-1, y) ++ [get m p] ++ getAfter m (x+1, y)

adjacentNumbers :: Matrix -> Pos -> [String]
adjacentNumbers m (x,y) = [topLeft, top, topRight, left, right, bottomLeft, bottom, bottomRight]
  where topLeft = if isNmb m (x-1, y-1) then getNumber m (x-1, y-1) else []
        top = if isNmb m (x, y-1) then getNumber m (x, y-1) else []
        topRight = if isNmb m (x+1, y-1) then getNumber m (x+1, y-1) else []
        left = if isNmb m (x-1, y) then getNumber m (x-1, y) else []
        right = if isNmb m (x+1, y) then getNumber m (x+1, y) else []
        bottomLeft = if isNmb m (x-1, y+1) then getNumber m (x-1, y+1) else []
        bottom = if isNmb m (x, y+1) then getNumber m (x, y+1) else []
        bottomRight = if isNmb m (x+1, y+1) then getNumber m (x+1, y+1) else []