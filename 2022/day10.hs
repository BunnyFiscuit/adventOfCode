{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day10 where
import FileReader
import Data.List
import GHC.Natural

main :: IO ()
main = do
  contents <- readF' "test"
  let ops = parse contents
  -- print ops
  let stack  = buildStack ops 1 0
  -- print stack
  let sumSignals = sum [ c * v | (c,v) <- stack, c `elem` cycles]
  -- putStrLn $ "part 1: " ++ show sumSignals
  print (take 21 ops)
  let crt = buildCrt ops (0,1) initSprite initCRT
  let ls  = map length (fst crt)
  print ls
  putStrLn $ unlines (fst crt)
  print (snd crt)

operations = [Add 15,Add (-11),Add 6,Add (-3),Add 5,Add (-1),Add (-8),Add 13,Add 4,NoOp,Add (-1),Add 5,Add (-1),Add 5,Add (-1),Add 5,Add (-1),Add 5,Add (-1),Add (-35),Add 1,Add 24,Add (-19),Add 1,Add 16,Add (-11),NoOp,NoOp,Add 21,Add (-15),NoOp,NoOp,Add (-3),Add 9,Add 1,Add (-3),Add 8,Add 1,Add 5,NoOp,NoOp,NoOp,NoOp,NoOp,Add (-36),NoOp,Add 1,Add 7,NoOp,NoOp,NoOp,Add 2,Add 6,NoOp,NoOp,NoOp,NoOp,NoOp,Add 1,NoOp,NoOp,Add 7,Add 1,NoOp,Add (-13),Add 13,Add 7,NoOp,Add 1,Add (-33),NoOp,NoOp,NoOp,Add 2,NoOp,NoOp,NoOp,Add 8,NoOp,Add (-1),Add 2,Add 1,NoOp,Add 17,Add (-9),Add 1,Add 1,Add (-3),Add 11,NoOp,NoOp,Add 1,NoOp,Add 1,NoOp,NoOp,Add (-13),Add (-19),Add 1,Add 3,Add 26,Add (-30),Add 12,Add (-1),Add 3,Add 1,NoOp,NoOp,NoOp,Add (-9),Add 18,Add 1,Add 2,NoOp,NoOp,Add 9,NoOp,NoOp,NoOp,Add (-1),Add 2,Add (-37),Add 1,Add 3,NoOp,Add 15,Add (-21),Add 22,Add (-6),Add 1,NoOp,Add 2,Add 1,NoOp,Add (-10),NoOp,NoOp,Add 20,Add 1,Add 2,Add 2,Add (-6),Add (-11),NoOp,NoOp,NoOp]

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

buildCrt :: [Op] -> (Cycle, Value) -> Sprite -> CRT -> (CRT, (Cycle, Value))
buildCrt [] cv _ crt = (crt, cv)

buildCrt (NoOp :ops) (c,v) sp crt = buildCrt ops (c+1,v) sp crt'
  where crt' = replace (crtIndex c) row' crt
        row  = crt !! crtIndex c
        row' = take c row ++ [sp !! mod c 39]

buildCrt (Add n:ops) (c,v) sp crt = buildCrt ops (c+2,v+n) sp' crt''
  where ci    = crtIndex c
        row   = crt !! ci ++ [sp !! mod c 39]
        crt'  = replace ci row crt
        ci'   = crtIndex (c+1)
        row'  = crt' !! ci' ++ [sp !! mod (c+1) 39]
        crt'' = replace ci' row' crt'
        sp'   = moveSprite (v+n)

replace :: Int -> String -> CRT -> CRT
replace n str crt = h ++ (str : drop 1 t)
  where (h,t) = splitAt n crt

moveSprite :: Int -> Sprite
moveSprite 0 = initSprite
moveSprite n = replicate (n-1) '.' ++ "###" ++ replicate (38-n) '.'

drawIt :: [(Cycle, Value)] -> (CRT, Sprite) -> (CRT, Sprite)
drawIt xs crtsp = foldl (flip draw) crtsp xs

draw :: (Cycle, Value) -> (CRT, Sprite) -> (CRT,Sprite)
draw (c,v) (crt, sp) = (crt', msp)
  where ci   = crtIndex (c-1)
        msp  = moveSprite v
        crt' = replace ci row' crt
        row  = crt !! ci
        row' = row ++ [msp !! (mod c 39)]

crtIndex :: Int -> Int
crtIndex c
  | c < 40    = 0
  | c < 80    = 1
  | c < 120   = 2
  | c < 160   = 3
  | c < 200   = 4
  | otherwise = 5


ex, ex2 :: [(Int, Int)]
ex = [
  (1,1),(2,1),(3,16),(4,16),(5,5),(6,5),(7,11),(8,11),(9,8),(10,8),(11,13),(12,13),(13,12),(14,12),(15,4),(16,4),(17,17),(18,17),(19,21),(20,21),(21,21),(22,20),(23,20),(24,25),(25,25),(26,24),(27,24),(28,29),(29,29),(30,28),(31,28),(32,33),(33,33),(34,32),(35,32),(36,37),(37,37),(38,36),(39,36),
  (40,1),(41,1),(42,2),(43,2)
  ]
ex2 = [(1,1),(2,1),(3,16),(4,16),(5,5),(6,5),(7,11),(8,11),(9,8)]
ini = (initCRT, initSprite)