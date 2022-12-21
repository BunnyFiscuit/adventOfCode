{-# LANGUAGE NamedFieldPuns #-}
module Day12 where
import FileReader
import Data.List
import Data.Maybe

alphs = ['a'..'z']
ex = ["Sabqponm","abcryxxl","accszExk","acctuvwj","abdefghi"]
s = (0,0)
e = (2,5)

type Point = (Int, Int)
type Row   = [Int]
type Path  = [Int]
type Grid  = [[Int]]
data RGrid  = G {
  grid    :: Grid,
  path    :: [Path],
  visited :: [Point],
  current :: Point,
  start   :: Point,
  end     :: Point
} deriving (Show)

initGrid :: [String] -> RGrid
initGrid input = G {
  grid    = parseAll input,
  path    = [],
  visited = [findLetter 'S' input (0,0)],
  current = findLetter 'S' input (0,0),
  start   = findLetter 'S' input (0,0),
  end     = findLetter 'E' input (0,0)
}

main :: IO ()
main = do
  contents <- readF' "12"
  print contents
  let gridRecord = initGrid contents
  putStrLn $ "Start: " ++ show (start gridRecord)
  putStrLn $ "End  : " ++ show (end gridRecord)
  mapM_ print (grid gridRecord)

findPath :: RGrid -> [Path]
findPath g@G{grid, path, current, start, end} = undefined
  where viables = viablePaths g

viablePaths :: RGrid -> [Point]
viablePaths G{grid, visited, current} = ps
  where ps    = concat [up, down, left, right]
        vl    = getVal grid current
        up    = [add current (0,-1) | canVisit grid vl (add current (0 ,-1)) && notElem (add current (0, - 1)) visited]
        down  = [add current (0,1)  | canVisit grid vl (add current (0 , 1)) && notElem (add current (0,   1)) visited]
        left  = [add current (-1,0) | canVisit grid vl (add current (-1, 0)) && notElem (add current (-1,  0)) visited]
        right = [add current (1,1)  | canVisit grid vl (add current (1 , 0)) && notElem (add current (1,   0)) visited]

closer :: Point -> Point -> Point -> Bool
closer a b end = undefined -- compare two points so see which one is closer to the end

findLetter :: Char -> [String] ->  Point -> Point
findLetter _ [] _     = (-1,-1)
findLetter c (x:xs) (row,col) = case c `elemIndex` x of
  Nothing -> findLetter c xs (row+1, col)
  Just n -> (row, n)

parseAll :: [String] -> [[Int]]
parseAll = map parseS

parseS :: String -> Row
parseS [] = []
parseS ('S':xs) = 1 : parseS xs
parseS ('E':xs) = 26 : parseS xs
parseS (x  :xs) = fromJust (elemIndex x alphs) + 1 : parseS xs

canVisit :: Grid -> Int -> Point -> Bool
canVisit grid v (x,y)
  | x < 0 || x > rows    = False
  | y < 0 || y > columns = False
  | otherwise            = abs (v - v') <= 1
  where v'      = getVal grid (x,y)
        rows    = length grid
        columns = length (head grid)

add :: Point -> Point -> Point
add (x,y) (a,b) = (x+a,y+b)

getVal :: Grid -> Point -> Int
getVal grid (x,y) = (grid !! x) !! y