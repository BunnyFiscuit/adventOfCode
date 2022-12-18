module Day18 where
import FileReader
import Data.List

data Cube  = C Int Int Int deriving (Show, Read)
type Faces = Int

instance Eq (Cube) where
  (==) (C a b c) (C x y z) = a == x && b == y && c == z

ex = [C 1 1 1,C 2 1 1]

main :: IO ()
main = do
  contents <- readF' "18"
  let cubes = map parseCube contents
  -- print cubes
  let rs = map (\x -> check cubes x 6) cubes
  print (sum rs)

parseCube :: String -> Cube
parseCube xs = C x y z
  where sp = wordsWhen (==',') xs 
        x  = read (sp !! 0) :: Int
        y  = read (sp !! 1) :: Int
        z  = read (sp !! 2) :: Int

check :: [Cube] -> Cube -> Faces -> Faces
check [] _ f = f
check xs c f = f - (adjX + adjY + adjZ)
  where adjX = checkX xs c
        adjY = checkY xs c
        adjZ = checkZ xs c


checkX :: [Cube] -> Cube -> Faces
checkX [] c = 0
checkX xs (C x y z) = xa + xb
  where xa = if C (x-1) y z `elem` xs then 1 else 0
        xb = if C (x+1) y z `elem` xs then 1 else 0

checkY :: [Cube] -> Cube -> Faces
checkY [] c = 0
checkY xs (C x y z) = xa + xb
  where xa = if C x (y-1) z `elem` xs then 1 else 0
        xb = if C x (y+1) z `elem` xs then 1 else 0

checkZ :: [Cube] -> Cube -> Faces
checkZ [] c = 0
checkZ xs (C x y z) = xa + xb
  where xa = if C x y (z-1) `elem` xs then 1 else 0
        xb = if C x y (z+1) `elem` xs then 1 else 0
