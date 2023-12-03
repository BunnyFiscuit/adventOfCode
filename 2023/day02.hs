module Day2 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "2"
  let x = map partOne contents
  --putStrLn $ show x
  putStrLn $ show $ sum x 
  let y = map partTwo contents
  --putStrLn $ show y
  putStrLn $ show $ sum y

type R = Int
type G = Int
type B = Int
type Id = Int
data Game = Game Id R G B deriving (Show)

partOne :: String -> Int
partOne s = if (all possible res) then id else 0
  where id   = read $ takeWhile (/= ':') (dropWhile (/= ' ') s) :: Int
        filt = drop 1 $ dropWhile (/= ':') s
        res = map (countCubes (0,0,0) . pairCubes) (map words (toSets filt))

partTwo :: String -> Int
partTwo s = r*g*b
  where fs = drop 1 $ dropWhile (/= ':') s
        res = map (countCubes' (0,0,0) . pairCubes) (map words (toSets fs))
        (r,g,b) = maxIt res

toSets :: String -> [String]
toSets [] = []
toSets s  = (takeWhile (/= ';')) s : toSets (drop 1 (dropWhile (/= ';') s))

possible :: (R,G,B) -> Bool
possible (r,g,b) = r <= 12 && g <= 13 && b <= 14

pairCubes :: [String] -> [(Int, String)]
pairCubes [] = []
pairCubes (x:y:xs) = (read x :: Int, y) : pairCubes xs

countCubes :: (R,G,B) -> [(Int, String)] -> (R,G,B)
countCubes (r,g,b) []  = (r,g,b)
countCubes (r,g,b) ((n,s):xs) 
  | "red" `isPrefixOf` s = countCubes (r+n, g, b) xs 
  | "green" `isPrefixOf` s = countCubes (r, g+n, b) xs
  | "blue" `isPrefixOf` s = countCubes (r, g, b+n) xs 

countCubes' :: (R,G,B) -> [(Int, String)] -> (R,G,B)
countCubes' (r,g,b) []  = (r,g,b)
countCubes' (r,g,b) ((n,s):xs) 
  | "red" `isPrefixOf` s = countCubes' (max r n, g, b) xs 
  | "green" `isPrefixOf` s = countCubes' (r, max g n, b) xs
  | "blue" `isPrefixOf` s = countCubes' (r, g, max b n) xs 

maxIt :: [(R,G,B)] -> (R,G,B)
maxIt [] = (0,0,0)
maxIt ((r,g,b):xs) = (max r r', max g g', max b b')
  where (r',g',b') = maxIt xs
