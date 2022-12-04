module Day11 where
import FileReader
import Data.List
import Data.Char
import Debug.Trace
import Control.Monad.State

main :: Int -> IO ()
main n = do
  ls <- readF 11
  let ms = map (map digitToInt) ls
  eval n ms
  
main2 :: IO ()
main2 = do
  ls <- readF 11
  let ms = map (map digitToInt) ls
  eval2 ms

type Matrix = [[Int]]
type Pos = (Int, Int)

showM :: Matrix -> IO ()
showM mx = do
  putStrLn $ str (toStrArr mx)

example :: Matrix
example = [
  [1,1,1,1,1],
  [1,9,9,9,1],
  [1,9,1,9,1],
  [1,9,9,9,1],
  [1,1,1,1,1]
  ]

ex2 :: Matrix
ex2 = [[5,4,8,3,1,4,3,2,2,3],[2,7,4,5,8,5,4,7,1,1],[5,2,6,4,5,5,6,1,7,3],[6,1,4,1,3,3,6,1,4,6],[6,3,5,7,3,8,5,4,7,8],[4,1,6,7,5,2,4,6,4,5],[2,1,7,6,8,4,1,7,2,1],[6,8,8,2,8,8,1,1,3,4],[4,8,4,6,8,4,8,5,5,4],[5,2,8,3,7,5,1,5,2,6]]

tex2 :: Matrix
tex2 = [[7,6,10,5,3,6,5,4,4,5],[4,9,6,7,10,7,6,9,3,3],[7,4,8,6,7,7,8,3,9,5],[8,3,6,3,5,5,8,3,6,8],[8,5,7,9,5,10,7,6,9,10],[6,3,8,9,7,4,6,8,6,7],[4,3,9,8,10,6,3,9,4,3],[8,10,10,4,10,10,3,3,5,6],[6,10,6,8,10,6,10,7,7,6],[7,4,10,5,9,7,3,7,4,8]]

step1 :: Matrix
step1 = [[2,2,2,2,2],[2,10,10,10,2],[2,10,2,10,2],[2,10,10,10,2],[2,2,2,2,2]]

eval :: Int -> Matrix -> IO ()
eval n mx = do
  -- putStrLn $ "Initial State:" 
  -- putStrLn $ str (toStrArr mx)
  let (res, v) = execState (run n) (mx,0)
  putStrLn $ "After " ++ show n ++ " steps: "
  putStrLn $ str (toStrArr res)
  putStrLn $ "Total of " ++ show v ++ " flashes"

eval2 :: Matrix -> IO ()
eval2 mx = do
  let (res, v) = execState allFlash (mx,0)
  putStrLn $ "All flash after " ++ show v ++ " steps!"
  putStrLn $ str (toStrArr res)
  
type StateM2 a = State Matrix a

allFlash :: StateM Int
allFlash = do
  (mx,step) <- get
  let octos = length (head mx) * length mx
  let ff = length $ (filter (==0) (concat mx))
  case ff == octos of
    True  -> return step
    False -> do
      let mx' = tick mx
      put (mx', step+1)
      ripple'
      allFlash 

type StateM a = State (Matrix,Int) a
startState = (example, 0)

run :: Int -> StateM Int
run 0 = do
  (mx, v) <- get
  return v

run n = do
  (mx, v) <- get
  put (tick mx, v)
  ripple
  run (n-1)

toStrArr mx = map unwords (map (map show) mx)
str [] = ""
str (x:xs) = x ++ "\n" ++ str xs

tick :: Matrix -> Matrix
tick mx = map (map (+1)) mx

ripple' :: StateM Matrix
ripple' = do
  (mx, v) <- get
  let end = (length (head mx), length mx)
  let ps = findFlashes mx
  let adjs = fInvalid end (concat (map adjPos ps))
  put (reset mx, v)
  case adjs of 
    [] -> do
      (mx, v) <- get
      let mx' = reset mx
      return mx'
    _  -> do
      ip <- incPos adjs
      put (ip, v)
      ripple'
  

ripple :: StateM (Matrix, Int)
ripple = do
  (mx, v) <- get
  let end = (length (head mx), length mx)
  let ps = findFlashes mx
  let adjs = fInvalid end (concat (map adjPos ps))
  put (reset mx, v)
  case adjs of 
    [] -> do
      (mx, v) <- get
      let mx' = reset mx
      put (mx', v + count mx')
      get
    _  -> do
      ip <- incPos adjs
      put (ip, v)
      ripple

incPos :: [Pos] -> StateM Matrix
incPos [] = get >>= \(mx, _) -> return mx
incPos ((x,y):ps) = do
  (mx, v) <- get
  let val = (mx !! y) !! x
  let val' = if val == 0 then 0 else val+1
  let mx' = insertM (x,y) val' mx
  put (mx', v)
  incPos ps

count :: Matrix -> Int
count = length . filter (==0) . concat
  
insertAt :: Int -> a -> [a] -> [a]
insertAt i e xs = take i xs ++ [e] ++ drop (i+1) xs

insertM :: Pos -> Int -> Matrix -> Matrix
insertM (x,y) n mx = insertAt y (insertAt x n (mx !! y)) mx

reset :: Matrix -> Matrix
reset mx = [[if x > 9 then 0 else x | x <- l] | l <- mx]

findFlashes :: Matrix -> [Pos]
findFlashes [] = []
findFlashes mx = concat $ map (\y -> ffRow (mx !! y) y) [0..(length mx - 1)] 

ffRow :: [Int] -> Int -> [Pos]
ffRow rs y = filter (\x -> x /= (-1,-1)) ff
  where ff = [if (rs !! a) > 9 then (a,y) else (-1,-1) | a <- [0..(length rs - 1)]]

adjPos :: Pos -> [Pos]
adjPos (x,y) = [
  (x-1,y-1),(x,y-1),(x+1,y-1),
  (x-1,y),(x+1,y),
  (x-1,y+1),(x,y+1),(x+1,y+1)]

fInvalid :: Pos -> [Pos] -> [Pos]
fInvalid _    []     = []
fInvalid end@(endX,endY) ((x,y):ps)
  | x < endX && x >= 0 && y < endY && y >= 0 = (x,y) : fInvalid end ps
  | otherwise            = fInvalid end ps
