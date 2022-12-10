{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day10 where
import FileReader
import Data.List
import GHC.Natural

main :: IO ()
main = do
  contents <- readF' "test"
  let ops = parse contents
  --print ops
  let r1  = buildStack ops 1 0
  print r1
  let sumSignals = sum [ c * v | (c,v) <- r1, c `elem` cycles]
  putStrLn $ "part 1: " ++ show sumSignals

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

buildStack :: [Op] -> Int -> Cycle -> [(Cycle, Value)]
buildStack [] _ _ = []
buildStack (NoOp :ops) v c = (c+1, v) : buildStack ops v (c+1)
buildStack (Add n:ops) v c
  = (c+1, v) : (c+2, v) : buildStack ops (v+n) (c+2)


draw :: (Cycle, Value) -> (CRT, Sprite) -> (CRT,Sprite)
draw (c,v) (crt, sp) = (crt', moveSprite v)
  where crt' = replace (crtIndex (c-1)) row' crt
        row  = crt !! (c-1)
        row' = row ++ [sp !! v]

ex :: [(Int, Int)]
ex = [(1,1),(2,1),(3,16),(4,16),(5,5)]

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