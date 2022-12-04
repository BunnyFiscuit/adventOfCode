module Day5 where

import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace
import Data.List (nub)

main :: IO ()
main = do
  contents <- readFile "inputs/5.txt"
  let ls = lines contents
  let red = map reader ls
  let fil = filter validDir red
  let lines = concat $ map line2List fil
  c <- run empty lines
  putStrLn $ show c

type MLines = M.Map Pos Int

empty :: MLines
empty = M.empty

insert :: Pos -> MLines -> MLines
insert pos m = case M.lookup pos m of
  Just v -> M.insert pos (v+1) m
  Nothing -> M.insert pos 1 m

run :: MLines -> [Pos] -> IO Int
run m [] = do
  return $ length (filter (\(p,c) -> c >= 2) (M.toList m))
run m (x:xs) = run (insert x m) xs

type Pos = (Int, Int)
data Line = H Pos Pos | V Pos Pos | D Pos Pos
  deriving (Show, Eq)

reader :: String -> Line
reader str = dir (str2Pos ftwo) (str2Pos ltwo)
  where ftwo = takeWhile (/=' ') str
        ltwo = reverse $ takeWhile (/=' ') (reverse str)

str2Pos :: String -> Pos
str2Pos xs = (read x :: Int, read y :: Int)
  where x  = takeWhile (/=',') xs
        y  = drop 1 (dropWhile (/=',') xs)

dir :: Pos -> Pos -> Line
dir p1@(x1,y1) p2@(x2,y2) = 
  if x1 == x2 then (V p1 p2) else
  if y1 == y2 then (H p1 p2) else (D p1 p2)

validDir :: Line -> Bool
validDir l = (c1 || c2 || c3)
  where (from@(x1,y1), to@(x2, y2)) = toPos l
        c1 = fst from == fst to
        c2 = snd from == snd to
        c3 = (hiy-loy) == (hix-lox)
        (hix, lox) = (max x1 x2, min x1 x2)
        (hiy, loy) = (max y1 y2, min y1 y2)

toPos :: Line -> (Pos, Pos)
toPos (H p1 p2) = (p1, p2)
toPos (V p1 p2) = (p1, p2)
toPos (D p1 p2) = (p1, p2)

line2List :: Line -> [Pos]
line2List l@(H from to) = zip [lo..hi] (repeat (snd from))
  where (x1, x2) = (fst from, fst to)
        (hi, lo) = (max x1 x2, min x1 x2)

line2List l@(V from to) = zip (repeat (fst to)) [lo..hi]
  where (y1, y2) = (snd from, snd to)
        (hi, lo) = (max y1 y2, min y1 y2)

line2List l@(D (x1,y1) (x2,y2)) = zip xs ys
  where (hix, lox) = (max x1 x2, min x1 x2)
        (hiy, loy) = (max y1 y2, min y1 y2)
        xs         = if x1 > x2 then (reverse [lox..hix]) else [lox..hix]
        ys         = if y1 > y2 then (reverse [loy..hiy]) else [loy..hiy]

