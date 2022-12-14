module Day12 where
import FileReader
import Data.List
import Data.Maybe

alphs = ['a'..'z']

main :: IO ()
main = do
  contents <- readF' "12"
  print contents
  let s = findLetter 'S' contents (0,0)
  let e = findLetter 'E' contents (0,0)
  putStrLn $ "Start: " ++ show s
  putStrLn $ "End  : " ++ show e
  let ss = parseAll contents
  print ss

type Point = (Int, Int)
type Grid  = [[Int]]
type Row   = [Int]
type Path  = [Int]

findLetter :: Char -> [String] ->  Point -> Point
findLetter _ [] _     = (-1,-1)
findLetter c (x:xs) (row,col) = case c `elemIndex` x of
  Nothing -> findLetter c xs (row+1, col)
  Just n -> (row, n)

parseAll :: [String] -> Grid
parseAll = map parseS

parseS :: String -> Row
parseS [] = []
parseS ('S':xs) = 1 : parseS xs
parseS ('E':xs) = 26 : parseS xs
parseS (x  :xs) = fromJust (elemIndex x alphs) + 1 : parseS xs

canVisit :: Grid -> Int -> Point -> Bool
canVisit grid v (x,y) = abs (v - rowVal) <= 1
  where row    = grid !! x
        rowVal = row !! y

findPath :: Grid -> [Path] -> Point -> Point -> [Path]
findPath grid ps current end = undefined

viablePaths :: Grid -> Point -> [Point]
viablePaths grid cur = ps
  where ps    = concat [up, down, left, right]
        vl    = getVal grid cur
        up    = [add cur (0,-1) | canVisit grid vl (add cur (0,-1))]
        down  = [add cur (0,1) | canVisit grid vl (add cur (0,1))]
        left  = [add cur (-1,0) | canVisit grid vl (add cur (-1,0))]
        right = [add cur (1,1) | canVisit grid vl (add cur (1,0))]

add :: Point -> Point -> Point
add (x,y) (a,b) = (x+a,y+b)

getVal :: Grid -> Point -> Int
getVal grid (x,y) = (grid !! x) !! y