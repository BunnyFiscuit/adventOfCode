{-# LANGUAGE NamedFieldPuns #-}
module Day12 where
import FileReader
import Data.List
import Data.Maybe
import Data.Char

alphs = ['a'..'z']
ex = ["Sabqponm","abcryxxl","accszExk","acctuvwj","abdefghi"]
s = (0,0)
e = (2,5)

type Point = (Int, Int)
type Row   = [Int]
type Path  = [Point]
type Grid  = [[Int]]
data RGrid  = G {
  grid    :: Grid,
  path    :: Path,
  visited :: [Point],
  current :: Point,
  start   :: Point,
  end     :: Point
} deriving (Show)

initGrid :: [String] -> RGrid
initGrid input = G {
  grid    = g,
  path    = [],
  visited = [s],
  current = s,
  start   = s,
  end     = gFind isEnd g
} where g = map (map ord) input
        s = gFind isStart g

printGrid :: Grid -> IO ()
printGrid g = do
  mapM_ (putStrLn . show) g

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

gFind :: (Int -> Bool) -> Grid -> Point
gFind f g = head [(x,y) | y <- [0..length g-1], x <- [0..length (head g)-1], f (getVal g (x,y))]

isStart :: Int -> Bool
isStart = (==83)

isEnd :: Int -> Bool
isEnd = (==69)

canVisit :: Grid -> Int -> Point -> Bool
canVisit grid v (x,y)
  | x < 0 || x > rows      = False
  | y < 0 || y > columns   = False
  | isStart v' || isEnd v' = True
  | otherwise              = abs (v - v') <= 1
  where v'      = getVal grid (x,y)
        rows    = length grid
        columns = length (head grid)

add :: Point -> Point -> Point
add (x,y) (a,b) = (x+a,y+b)

getVal :: Grid -> Point -> Int
getVal grid (x,y) = (grid !! y) !! x

main :: IO ()
main = do
  contents <- readF' "12"
  print contents
  let gridRecord = initGrid contents
  putStrLn $ "Start: " ++ show (start gridRecord)
  putStrLn $ "End  : " ++ show (end gridRecord)
  mapM_ print (grid gridRecord)