module Day9 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "9"
  let dirs = map read contents :: [Direction]
  --print dirs
  let rs = run (initRope 2) dirs [initRope 2]
  -- print (nub (getTails rs))
  let res = length (nub (getTails rs))
  -- "part 1: 6470"
  putStrLn $ "part 1: " ++ show res

  let rs'  = run (initRope 10) dirs [initRope 10]
  let res' = length (nub (getTails rs'))
  putStrLn $ "part 2: " ++ show res' 

data Direction = U Int | D Int | L Int | R Int deriving (Show, Read)

type Pos = (Int, Int)
type Head = Pos
type Tail = Pos
type Rope = [Pos]


initRope :: Int -> [(Int, Int)]
initRope n = replicate n (0,0)

rep  = initRope 2
rep' = initRope 10

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
move r d = if getN d == 0 then [] else move' r d

move' :: Rope -> Direction -> [Rope]
move' r (U n) = mv' : move mv' (U (n-1))  -- up
  where mv' = mv r (0,-1)

move' r (D n) = mv' : move mv' (D (n-1))  -- down
  where mv' = mv r (0,1)

move' r (L n) = mv' : move mv' (L (n-1))  -- left
  where mv' = mv r (-1,0)

move' r (R n) = mv' : move mv' (R (n-1))  -- right
  where mv' = mv r ( 1,0)

mv :: Rope -> Pos -> Rope
mv [] _  = []
mv [h] p = [add h p]
mv (h:t:knots) p
  | df `elem` validDists = h' : t : knots
  | otherwise            = adjust (h':t:knots)
  where h' = add h p
        df = diff h' t

adjust :: Rope -> Rope
adjust [] = []
adjust [k] = [k]
adjust (k1@(x,y) : k2@(a,b):knots)
  | abs dx  > 1 && abs dy > 1 = k1 : knots' (a + m dx  , b + m dy)
  | abs dx == 1 && abs dy > 1 = k1 : knots' (a + m dx  , b + m dy)
  | abs dy == 1 && abs dx > 1 = k1 : knots' (a + m dx, b + m dy)
  | dy     == 0 && abs dx > 1 = k1 : knots' (a + m dx, b)
  | dx     == 0 && abs dy > 1 = k1 : knots' (a, b + m dy)
  | otherwise                 = k1 : knots' k2
  where (dx,dy) = (x-a, y-b)
        knots' k2'  = if null knots then [k2'] else adjust (k2':knots)
        m p
          | p > 0 = 1
          | p < 0 = -1
          | otherwise = 0

validDists = [(x,y) | x <- [0,1], y <- [0,1]]

add :: Pos -> Pos -> Pos
add (x,y) (u,v) = (x+u, y+v)

diff :: Pos -> Pos -> Pos
diff (x,y) (a,b) = (abs (x-a), abs (y-b))

-- type Rope = [(Int,Int)]
getTails :: [Rope] -> [Pos]
getTails xs = map last xs