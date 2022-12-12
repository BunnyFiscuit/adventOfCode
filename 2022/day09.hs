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
data Rope = Rope Head Tail deriving (Show, Read)

initRope = Rope (0,0) (0,0)

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
move' r (U n) = up r n    : move (up r n) (U (n-1))
move' r (D n) = down r n  : move (down r n) (D (n-1))
move' r (L n) = left r n  : move (left r n) (L (n-1))
move' r (R n) = right r n : move (right r n) (R (n-1))

adjust :: Rope -> Rope
adjust r@(Rope (x,y) (a,b))
  | abs dx == 1 && abs dy > 1 = Rope (x,y) (a + dx, b + m dy) -- move dx and one dy
  | abs dy == 1 && abs dx > 1 = Rope (x,y) (a + m dx, b + dy) -- move dy and one dx
  | dy     == 0 && abs dx > 1 = Rope (x,y) (a + m dx, b)
  | dx     == 0 && abs dy > 1 = Rope (x,y) (a, b + m dy)
  | otherwise                 = r
  where (dx,dy) = (x-a, y-b)
        m x
          | x > 0 = 1
          | x < 0 = -1
          | otherwise = 0

validDists = [(x,y) | x <- [0,1], y <- [0,1]]

up :: Rope -> Int -> Rope
up r 0          = r
up (Rope h@(x,y) t@(a,b)) n
  | df `elem` validDists = Rope h' t
  | otherwise              = adjust (Rope h' t)
  where h' = (x, y-1)
        df = diff h' t

down :: Rope -> Int -> Rope
down r 0          = r
down (Rope h@(x,y) t@(a,b)) n
  | df `elem` validDists = Rope h' t
  | otherwise         = adjust (Rope h' t)
  where h' = (x, y+1)
        df = diff h' t

left :: Rope -> Int -> Rope
left r 0          = r
left (Rope h@(x,y) t@(a,b)) n
  | df `elem` validDists = Rope h' t
  | otherwise         = adjust (Rope h' t)
  where h' = (x-1, y)
        df = diff h' t

right :: Rope -> Int -> Rope
right r 0          = r
right (Rope h@(x,y) t@(a,b)) n
  | df `elem` validDists = Rope h' t
  | otherwise            = adjust (Rope h' t)
  where h' = (x+1, y)
        df = diff h' t

diff :: Pos -> Pos -> Pos
diff (x,y) (a,b) = (abs (x-a), abs (y-b))

getTails :: [Rope] -> [Pos]
getTails [] = []
getTails ((Rope _ t):rs) = t : getTails rs