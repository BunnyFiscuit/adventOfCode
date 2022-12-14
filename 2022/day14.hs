{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day14 where
import FileReader
import Data.List
import Data.Maybe

main :: IO ()
main = do
  contents <- readF' "14"
  let rocks = nub (concatMap (createRocks . words) contents)
  let offset = minimum (map fst rocks)
  print offset
  let rocks' = map (\(x,y) -> (y, x-offset)) rocks
  print rocks'
  sand <- run rocks' [] offset
  let maxX  = maximum (map fst rocks')
  let maxY  = maximum (map snd rocks')
  let grid  = [if (x,y) `elem` rocks' then Rock else if (x,y) `elem` sand then Sand else Air | x <- [0..maxX], y <- [0..maxY]]
  let spaceGrid = groupIt grid (maxX+1)
  mapM_ (putStrLn . showSpace) spaceGrid
  print (length sand)

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

run :: [Rock] -> [Sand] -> Int -> IO [Sand]
run rocks sand offset = do
  sand' <- tick rocks sand offset
  if sand' == sand then return sand else run rocks sand' offset


type Rock  = (Int, Int)
type Sand  = (Int, Int)
type Grain = (Int, Int)

data Space = Sand | Rock | Air deriving (Show, Eq, Read)

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

tick :: [Rock] -> [Sand] -> Int -> IO [Sand]
tick rocks sand offset = do
  maybeG <- tick' rocks sand (0,500-offset)
  case maybeG of
    Nothing -> return sand
    Just g  -> return $ g : sand

tick' :: [Rock] -> [Sand] -> Grain -> IO (Maybe Grain)
tick' rocks sand grain@(x,y)
  | x < leftMost || x > rightMost || y > downMost = do
    --print grain
    --print $ "x < leftMost || x > rightMost || y > downMost -- " ++ show (x < leftMost || x > rightMost || y > downMost)
    return Nothing

  | safeDown             = do
    --print grain
    --print ("safeDown: " ++ show safeDown)
    tick' rocks sand down

  | sandDown && safeLeft = do
    --print grain
    --print ("safeDown && safeLeft: " ++ show (safeDown && safeLeft))
    tick' rocks sand downLeft

  | sandDown && not safeLeft && safeRight = tick' rocks sand downRight

  | otherwise = return $ Just grain
  where down      = addDown grain
        downLeft  = addDownLeft grain
        downRight = addDownRight grain
        safeDown  = check rocks sand down == Air
        sandDown  = check rocks sand down == Sand
        safeLeft  = check rocks sand downLeft == Air
        safeRight = check rocks sand downRight == Air
        leftMost  = minimum (map fst rocks)
        rightMost = maximum (map fst rocks)
        downMost  = maximum (map snd rocks)

check :: [Rock] -> [Sand] -> Grain -> Space
check rocks sand grain
  | rockDown  = Rock
  | sandDown  = Sand
  | otherwise = Air
  where rockDown  = isJust (grain `elemIndex` rocks)
        sandDown  = isJust (grain `elemIndex` sand )

addDown :: Grain -> Grain
addDown (x,y) = (x+1,y)

addDownLeft :: Grain -> Grain
addDownLeft (x,y) = (x+1,y-1)

addDownRight :: Grain -> Grain
addDownRight (x,y) = (x+1,y+1)

rs :: [(Int, Int)]
rs = [(498,4),(498,5),(498,6),(496,6),(497,6),(502,4),(503,4),(502,5),(502,6),(502,7),(502,8),(502,9),(494,9),(495,9),(496,9),(497,9),(498,9),(499,9),(500,9),(501,9)]