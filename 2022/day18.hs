module Day18 where
import FileReader
import Data.List

type Cube = (Int, Int, Int)
type Faces = Int


ex :: [Cube]
ex = [(1, 1, 1), (2, 1, 1)]

main :: IO ()
main = do
  contents <- readF' "18"
  let cubes = map parseCube contents
  -- print cubes
  let rs = map (\x -> check cubes x 6) cubes
  putStrLn $ "part 1: " ++ show (sum rs)
  let ((minX, minY, minZ),(maxX, maxY, maxZ)) = dims cubes
  let possiblePockets = [(x,y,z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]] \\ cubes
  let pockets = [pocket | pocket <- possiblePockets, isPocket cubes pocket]
  print pockets
  let rs' = map (\x -> check cubes x 6) pockets
  print rs'
  putStrLn $ "part 2: " ++ show (sum rs - (sum rs'))

parseCube :: String -> Cube
parseCube xs = (x, y, z)
  where sp = wordsWhen (==',') xs 
        x  = read (sp !! 0) :: Int
        y  = read (sp !! 1) :: Int
        z  = read (sp !! 2) :: Int

check :: [Cube] -> Cube -> Faces -> Faces
check [] _ f = f
check cubes (x,y,z) f = f - vn
  where vn   = sum [1 | cube <- neighbors (x,y,z), cube `elem` cubes]

neighbors :: Cube -> [Cube]
neighbors (x,y,z) = [
  ((x-1), y, z), ((x+1), y, z),
  (x, (y-1), z), (x, (y+1), z),
  (x, y, (z-1)), (x, y, (z+1))]

dims :: [Cube] -> (Cube, Cube)
dims cubes = ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))
  where xs = map (\(x,_,_) -> x) cubes
        ys = map (\(_,y,_) -> y) cubes
        zs = map (\(_,_,z) -> z) cubes

isPocket :: [Cube] -> Cube -> Bool
isPocket cubes (x,y,z) = and [chX, chY, chZ]
  where chX = and [cube `elem` cubes | cube <- [(x-1, y, z) , (x+1, y, z)]]
        chY = and [cube `elem` cubes | cube <- [(x, y-1, z) , (x, y+1, z)]]
        chZ = and [cube `elem` cubes | cube <- [(x, y, z-1) , (x, y, z+1)]]