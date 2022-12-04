module Day8 where
import System.IO
import Data.List (sort, sortBy, group, groupBy, findIndex, isInfixOf)
import Debug.Trace
import Data.Maybe

main :: IO ()
main = do
  contents <- readFile "inputs/8.txt"
  let ls = lines contents
  let ws = map (str2Entry . words) ls
  -- print ws
  ms <- mapM (\(i,o) -> run i o) ws
  let xs = map (concat . map show) ms
  print (sum (map (\x -> read x :: Int) xs))

run :: [String] -> [String] -> IO [Int]
run inpt outpt = do
  -- print inpt
  sg <- runSG inpt
  let ms = map (decode sg) outpt
  return $ map fromJust (filter isJust ms)

runSG :: [String] -> IO Segments
runSG xs = return $ segment initSegments ezs xs

decode :: Segments -> String -> Maybe Int
decode sg o = findIndex (==(sort o)) (map sort sg)

type Entry = ([String], [String])
str2Entry :: [String] -> Entry
str2Entry s = (input,output)
  where input  = takeWhile (/="|") s
        output = drop 1 $ dropWhile (/="|") s 
        
easyNumbers :: String -> Bool
easyNumbers s = l == 2 || l == 3 || l == 4 || l == 7
  where l = length s

-- [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
type Segments = [String]

-- 1, 4, 7, 8
initSegments = replicate 10 "0"
one   sg (x:xs) = if length x == 2 then (insertAt 1 x sg) else one   sg xs  
four  sg (x:xs) = if length x == 4 then (insertAt 4 x sg) else four  sg xs
seven sg (x:xs) = if length x == 3 then (insertAt 7 x sg) else seven sg xs
eight sg (x:xs) = if length x == 7 then (insertAt 8 x sg) else eight sg xs

three sg (x:xs) = if (length (concat f) == 2) then (insertAt 3 x sg) else three sg xs 
  where sone = sg !! 1
        f = map (\a -> filter (==(sone !! a)) x) [0,1]

fivesix sg xs  = six fg fs
  where sone = sg !! 1
        f x  = map (\a -> filter (==(sone !! a)) x) [0,1]
        ss = map concat (map f xs)
        gg   = groupBy (\x y -> snd x == snd y) (mySort (zip xs ss))
        fs   = concat $ filter (\x -> length x == 2) gg
        fg   = five sg fs

five :: Segments -> [(String, String)] -> Segments
five sg (x:xs) = if length a == 5 then (insertAt 5 a sg) else five sg xs
  where a = fst x

six :: Segments -> [(String, String)] -> Segments
six  sg (x:xs) = if length a == 6 then (insertAt 6 a sg) else six sg xs
  where a = fst x

zero sg xs = insertAt 0 (find df ms) sg
  where df = diff (sort (sg !! 8)) (sort (sg !! 3))
        fs = remaining sg xs
        ms = zip fs (map sort fs)
        
find :: String -> [(String,String)] -> String
find a [] = "0"
find a ((i,j):bs) = if (isElem j) then i else find a bs  
  where isElem s = and $ map (\x -> isInfixOf [x] s) a  

two  sg xs = insertAt 2 (head f) sg
  where f = filter (\x -> length x == 5) xs

nine sg xs = insertAt 9 (head fs) sg
  where fs = remaining sg xs

remaining :: Segments -> [String] -> [String]
remaining sg xs = filter (\x -> not (elem x sg)) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i e xs = take i xs ++ [e] ++ drop (i+1) xs

-- Finder functions
type Finder = (Segments -> [String] -> [String])
ezs :: [Finder]
ezs = [one,four,seven,eight, -- 1,4,7,8
 (\sg xs -> fivesix (three sg (filter (\x -> length x == 5) xs)) xs), -- 1,3,4,5,6,7,8
 (\sg xs -> two sg (filter (\x -> not (elem x sg)) xs)), -- 1,2,3,4,5,6,7,8
 zero, nine] -- 0,1,2,3,4,5,6,7,8,9

segment :: Segments -> [Finder] -> [String] -> [String]
segment sg []      _ = sg
segment sg (f:fs) xs = segment sg' fs xs
  where sg' = f sg xs

myCompare :: (String,String) -> (String,String) -> Ordering
myCompare (a,b) (x,y) = if b > y then GT else LT

mySort = sortBy myCompare

diff :: String -> String -> String
diff [] to = []
diff (x:xs) to 
  | elem x to = diff xs to
  | otherwise = x : diff xs to

-- remove later
ws = words "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
ws2 = words "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec"
o  = "ab"
f x = map (\a -> filter (==(o !! a)) x) [0,1]