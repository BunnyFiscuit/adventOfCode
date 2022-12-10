module Day9 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "9"
  let dirs = map read contents :: [Direction]
  --print dirs
  let rs = run initRope dirs []
  --print rs
  let res = length (nub (getTails rs))
  putStrLn $ "part 1: " ++ show res

data Direction = U Int | D Int | L Int | R Int deriving (Show, Read)

type Pos = (Int, Int)
type Head = Pos
type Tail = Pos
type Rope = [Pos]


initRope = [(0,0), (0,0)]

run :: Rope -> [Direction] -> [Rope] -> [Rope]
run r [] rs     = rs
run r (d:ds) rs = run (last r') ds (rs ++ r')
  where r' = move r d

getN :: Direction -> Int
getN (U n) = n
getN (D n) = n
getN (L n) = n
getN (R n) = n

move :: Rope -> Direction -> [Rope]
move r d     = if getN d == 0 then [r] else move' r d

move' :: Rope -> Direction -> [Rope]
move' r (U n) = up r    : move (up r) (U (n-1))
move' r (D n) = down r n  : move (down r n) (D (n-1))
move' r (L n) = left r n  : move (left r n) (L (n-1))
move' r (R n) = right r n : move (right r n) (R (n-1))

adjust :: Rope -> Rope
adjust [] = []
adjust [k] = [k]
adjust (k1@(x,y) : k2@(a,b):knots)
  | abs dx == 1 && abs dy > 1 = (a + dx, b + m dy) : knots' 
  | abs dy == 1 && abs dx > 1 = (a + m dx, b + dy) : knots'
  | dy     == 0 && abs dx > 1 = (a + m dx, b)      : knots'
  | dx     == 0 && abs dy > 1 = (a, b + m dy)      : knots'
  | otherwise                 = [k1,k2] ++ knots'
  where (dx,dy) = (x-a, y-b)
        knots'  = if null knots then [] else adjust knots
        m p
          | p > 0 = 1
          | p < 0 = -1
          | otherwise = 0

validDists = [(x,y) | x <- [0,1], y <- [0,1]]

up :: Rope -> Rope
up []  = []
up [h] = [h]
up ((x,y):t:knots)
  | df `elem` validDists = h' : up (t:knots)
  | otherwise            = adjust (h':t:knots)
  where h' = (x,y-1)
        df = diff h' t

down :: Rope -> Int -> Rope
down r 0          = r
down (h@(x,y), t@(a,b))  n
  | df `elem` validDists = (h', t)
  | otherwise            = adjust (h', t)
  where h' = (x, y+1)
        df = diff h' t

left :: Rope -> Int -> Rope
left r 0          = r
left (h@(x,y), t@(a,b))  n
  | df `elem` validDists = (h', t)
  | otherwise         = adjust (h', t)
  where h' = (x-1, y)
        df = diff h' t

right :: Rope -> Int -> Rope
right r 0          = r
right (h@(x,y), t@(a,b))  n
  | df `elem` validDists = (h', t)
  | otherwise            = adjust (h', t)
  where h' = (x+1, y)
        df = diff h' t

diff :: Pos -> Pos -> Pos
diff (x,y) (a,b) = (abs (x-a), abs (y-b))

getTails :: [Rope] -> [Pos]
getTails [] = []
getTails ((h,t):rs) = t : getTails rs