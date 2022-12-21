{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day14 where
import FileReader
import Data.List
import Data.Maybe
import Control.Concurrent

type Rock  = (Int, Int)
type Sand  = (Int, Int)
type Grain = (Int, Int)

data Space = Sand | Rock | Air | SandStart deriving (Show, Eq, Read)

data Cave = Cave {
  rocks  :: [Rock],
  sand   :: [Sand],
  grain  :: Grain,
  offset :: Int
}

main :: IO ()
main = do
  contents <- readF' "14"
  let rocks = nub (concatMap (createRocks . words) contents)
  let offset = minimum (map fst rocks) - 5
  let yMax  = maximum (map snd rocks) + 2
  let xMax  = maximum (map fst rocks) + (500-offset)
  let floor = [(x, yMax) | x <- [(500-offset)-10..xMax]]
  let rocksWithFloor = rocks ++ floor
  --print rocks
  print offset
  let rocks' = map (\(x,y) -> (x-offset,y)) rocksWithFloor
  --print rocks'
  let initCave = Cave { rocks = rocks', sand = [], grain = (500-offset, 0), offset = offset }
  --drawGrid initCave
  sand' <- run initCave 0
  --drawGrid (initCave { sand = sand'})
  print (length sand')


drawGrid :: Cave -> IO ()
drawGrid Cave{rocks, sand, grain, offset} = do
  let maxX  = maximum (map fst rocks)
  let maxY  = maximum (map snd rocks)
  let grid  = [if (x,y) `elem` rocks then Rock else if (x,y) == (500-offset,0) then SandStart else if (x,y) `elem` sand then Sand else Air | y <- [0..maxY], x <- [-5..maxX]]
  let spaceGrid = groupIt grid (abs (-5 - maxX) + 1)
  --mapM_ print spaceGrid
  mapM_ (putStrLn . showSpace) spaceGrid
  putStrLn ""

run :: Cave -> Int -> IO [Sand]
run c i = do
  c' <- tick c
  let diff = sand c' \\ sand c
  if mod i 100 == 0 then putStr "." else pure ()
  --drawGrid c
  --threadDelay 250000
  if sand c' == sand c || diff == [(500 - offset c,0)] then do putStrLn ""; return (sand c') else run (c { sand = sand c'}) (i+1)

tick :: Cave -> IO Cave
tick cave = do
  maybeG <- tick' cave
  case maybeG of
    Nothing -> return cave
    Just g  -> return $ cave { sand = g : sand cave}

tick' :: Cave -> IO (Maybe Grain)
tick' c@Cave{rocks, sand, grain = g@(x,y), offset}
  | y >= downMost = do
    --print $ "x < leftMost || x > rightMost || y > downMost -- " ++ show (x < leftMost || x > rightMost || y > downMost)
    return Nothing

  | safeDown             = do
    tick' (c { grain = down})

  | not safeDown && safeLeft = do
    tick' (c { grain = downLeft})

  | not safeDown && not safeLeft && safeRight = do
    tick' (c { grain = downRight})

  | otherwise = do
    return $ Just g
  where down      = addDown g
        downLeft  = addDownLeft g
        downRight = addDownRight g
        safeDown  = check rocks sand down == Air
        safeLeft  = check rocks sand downLeft == Air
        safeRight = check rocks sand downRight == Air
        leftMost  = minimum (map fst rocks)
        rightMost = maximum (map fst rocks)
        downMost  = maximum (map snd rocks)

check :: [Rock] -> [Sand] -> Grain -> Space
check rocks sand grain@(x,y)
  | rockDown  = Rock
  | sandDown  = Sand
  | otherwise = Air
  where rockDown  = isJust (grain `elemIndex` rocks) || y == maximum (map snd rocks)
        sandDown  = isJust (grain `elemIndex` sand )
        downMost  = maximum (map snd rocks)

addDown :: Grain -> Grain
addDown (x,y) = (x,y+1)

addDownLeft :: Grain -> Grain
addDownLeft (x,y) = (x-1,y+1)

addDownRight :: Grain -> Grain
addDownRight (x,y) = (x+1,y+1)

groupIt :: [Space] -> Int -> [[Space]]
groupIt [] _ = []
groupIt xs t = x : groupIt xs' t
  where x   = take t xs
        xs' = drop t xs

showSpace :: [Space] -> String
showSpace [] = []
showSpace (Air:sp)  = '.' : showSpace sp
showSpace (Rock:sp) = '#' : showSpace sp
showSpace (Sand:sp) = 'o' : showSpace sp
showSpace (SandStart:sp) = '+' : showSpace sp

point :: String -> Rock
point xs = (read x :: Int, read (drop 1 y) :: Int)
  where (x,y) = break (==',') xs

createRocks :: [String]  -> [Rock]
createRocks []  = []
createRocks [x] = [point x]
createRocks (p1:"->":p2:rest)
  | ax == bx  = zip (repeat ax) [(min ay by)..(max ay by)] ++ createRocks (p2:rest)
  | otherwise = zip [(min ax bx)..(max ax bx)] (repeat ay) ++ createRocks (p2:rest)
  where (ax,ay) = point p1
        (bx,by) = point p2

rs :: [(Int, Int)]
rs = [(498,4),(498,5),(498,6),(496,6),(497,6),(502,4),(503,4),(502,5),(502,6),(502,7),(502,8),(502,9),(494,9),(495,9),(496,9),(497,9),(498,9),(499,9),(500,9),(501,9)]