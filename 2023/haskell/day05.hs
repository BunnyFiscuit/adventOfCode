module Day where
import FileReader
import Data.List
import qualified Data.Map as  M

type Seeds = [Int]
type DSMap = M.Map Int Int -- destination source map

main :: IO ()
main = do
  contents <- readF' "5"
  --putStrLn $ show contents
  let ss = take 1 contents
  let seeds = initSeeds ((drop 1 . words . head) ss)
  putStrLn $ "Seeds: " ++ show seeds
  let seedToSoil = getMap' contents "seed-to-soil map:"
  --putStrLn $ show $ M.toList seedToSoil
  let soilToFert = getMap' contents "soil-to-fertilizer map:"
  --putStrLn $ show (length soilToFert == 54)
  let fertToWater = getMap' contents "fertilizer-to-water map:"
  --putStrLn $ show (length fertToWater == 61)
  let waterToLight = getMap' contents "water-to-light map:"
  --putStrLn $ show (length waterToLight == 77)
  let lightToTemp = getMap' contents "light-to-temperature map:" 
  --putStrLn $ show (length lightToTemp == 55)
  let tempToHumid = getMap' contents "temperature-to-humidity map:"
  --putStrLn $ show (length tempToHumid == 70)
  let humidToLocation = getMap' contents "humidity-to-location map:"
  --putStrLn $ show (length humidToLocation == 41)
  let ms = [seedToSoil, soilToFert, fertToWater, waterToLight, lightToTemp, tempToHumid, humidToLocation]
  let ls = map (seedToLocation ms) seeds
  putStrLn $ show (minimum ls)


initSeeds :: [String] -> Seeds
initSeeds s = map read s

strToMap :: [String] -> DSMap
strToMap (d:s:r:[]) = M.fromList (zip [s'..(s'+r'-1)] [d'..(d'+r'-1)])
  where d' = read d :: Int
        s' = read s :: Int
        r' = read r :: Int

getMap' :: [String] -> String -> DSMap
getMap' xs s = M.unions two
  where one = drop 1 $ takeWhile (/="") (dropWhile (/=s) xs)
        two = map (\x -> strToMap (words x)) one

getMap :: [String] -> String -> [String]
getMap xs s = drop 1 $ takeWhile (/="") (dropWhile (/=s) xs)

get :: DSMap -> Int -> Int
get m i = case M.lookup i m of
  Just x -> x
  Nothing -> i

seedToLocation :: [DSMap] -> Int -> Int
seedToLocation [m] n = get m n
seedToLocation (m:ms) n = seedToLocation ms (get m n) 

soils :: DSMap
soils = M.fromList [(50,52),(51,53),(52,54),(53,55),(54,56),(55,57),(56,58),(57,59),(58,60),(59,61),(60,62),(61,63),(62,64),(63,65),(64,66),(65,67),(66,68),(67,69),(68,70),(69,71),(70,72),(71,73),(72,74),(73,75),(74,76),(75,77),(76,78),(77,79),(78,80),(79,81),(80,82),(81,83),(82,84),(83,85),(84,86),(85,87),(86,88),(87,89),(88,90),(89,91),(90,92),(91,93),(92,94),(93,95),(94,96),(95,97),(96,98),(97,99),(98,50),(99,51)]