module Day where
import FileReader
import Data.List
import qualified Data.Map as  M

main :: IO ()
main = do
  contents <- readF' "test"
  putStrLn $ show contents
  let ss = take 1 contents
  let seeds = initSeeds ((drop 1 . words . head) ss)
  putStrLn $ "Seeds: " ++ show seeds
  let seedToSoil = getMap contents "seed-to-soil map:"
  putStrLn $ "Seed to soil: " ++ show seedToSoil
  let soilToFert = map (\x -> strToMap (words x)) (getMap contents "soil-to-fertilizer map:")
  putStrLn $ "Soil to fertilizer: " ++ show soilToFert

type Seeds = [Int]
type SoilMap = M.Map Int Int

initSeeds :: [String] -> Seeds
initSeeds s = map read s

strToMap :: [String] -> SoilMap
strToMap (d:s:r:[]) = M.fromList (zip [s'..(s'+r'-1)] [d'..(d'+r'-1)])
  where d' = read d :: Int
        s' = read s :: Int
        r' = read r :: Int

initSoilMap :: [String] -> SoilMap
initSoilMap s = undefined

getMap :: [String] -> String -> [String]
getMap xs s = drop 1 $ takeWhile (/="") (dropWhile (/=s) xs)