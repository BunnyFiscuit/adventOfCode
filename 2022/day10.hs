{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day10 where
import FileReader
import Data.List
import GHC.Natural

main :: IO ()
main = do
  contents <- readF' "10"
  let ops = parse contents
  --print ops
  let stack  = buildStack ops 1 1
  --print stack
  let sumSignals = sum [ c * v | (c,v) <- stack, c `elem` cycles]
  putStrLn $ "part 1: " ++ show sumSignals
  
  putStrLn $ "part 2:"
  r2' <- bStack ops 0 1
  -- hard to read but: FPGPHFGH
  putStrLn $ unlines (splitRes (map snd r2'))

cycles = [20, 60, 100, 140, 180, 220]
initSprite = "###....................................."
initCRT    = ["","","","","",""]

data Op = NoOp | Add Int deriving (Show, Read)
type Value = Int
type Cycle = Int
type Sprite = String
type CRT    = [String]

parse :: [String] -> [Op]
parse [] = []
parse ("noop":ls)                  = NoOp : parse ls
parse (('a':'d':'d':'x':' ':n):ls) = Add (read n) : parse ls
parse (_:ls)                       = parse ls

buildStack :: [Op] -> Cycle -> Value  -> [(Cycle, Value)]
buildStack [] _ _ = []
buildStack (NoOp :ops) c v = (c, v) : buildStack ops (c+1) v
buildStack (Add n:ops) c v
  = (c, v) : (c+1, v) : buildStack ops (c+2) (v+n)

bStack :: [Op] -> Cycle -> Value -> IO [(Cycle, Char)]
bStack [] _ _ = return []
bStack (NoOp  : ops) c v = do
  next <- bStack ops (c+1) v
  return $ (c,ch) : next
  where ch = moveSprite v !! (mod c 40)
bStack (Add n : ops) c v = do
  next <- bStack ops (c+2) (v+n)
  return $ (c,ch) : (c+1, ch') : next 
  where sp  = moveSprite v
        ch  = sp !! (mod c 40)
        ch' = sp !! mod (c+1) 40

splitRes :: String -> [String]
splitRes [] = []
splitRes xs = take 40 xs : splitRes (drop 40 xs)

-- step through this. why aint it working?
draw :: (Cycle, Value) -> (CRT, Sprite) -> (CRT,Sprite)
draw (c,v) (crt, sp) = (crt', moveSprite v)
  where crt' = replace (crtIndex c) row' crt
        row  = crt !! crtIndex c
        row' = row ++ [sp !! mod c 40]

draw' :: [(Cycle, Value)] -> (CRT, Sprite) -> IO (CRT)
draw' [] (crt,_) = return crt
draw' ((c,v):cvs) (crt, sp) = do
  draw' cvs (crt', moveSprite v)
  where crt' = replace (crtIndex c) row' crt
        row  = crt !! crtIndex c
        row' = row ++ [sp !! mod c 40]

ex :: [(Int, Int)]
ex = [(1,1),(2,1),(3,16),(4,16),(5,5)]
ini = (initCRT, initSprite)

moveSprite :: Int -> Sprite
moveSprite 0 = initSprite
moveSprite n = replicate (n-1) '.' ++ "###" ++ replicate (38-n) '.'

replace :: Int -> String -> CRT -> CRT
replace n str crt = h ++ (str : drop 1 t)
  where (h,t) = splitAt n crt

crtIndex :: Int -> Int
crtIndex c
  | c < 40    = 0
  | c < 80    = 1
  | c < 120   = 2
  | c < 160   = 3
  | c < 200   = 4
  | otherwise = 5