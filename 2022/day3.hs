module Day3 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF 3
  let fpart1    = map (commons . toBackpack) contents
  putStrLn $ "part 1: " ++ show (sum (map prio fpart1))
  let fpart2    = map (\x -> tripleCommons x) (elfGroups contents)
  putStrLn $ "part 2: " ++ show (sum (map prio fpart2))

prio :: String -> Int
prio (c : []) = case elemIndex c (['a'..'z'] ++ ['A'..'Z']) of
  Just i  -> i+1
  Nothing -> 0
prio _        = 0

toBackpack :: String -> (String, String)
toBackpack s = splitAt ((length s) `div` 2) s

commons :: (String, String) -> String
commons (f, s) = nub (intersect f s)

tripleCommons :: (String, String, String) -> String
tripleCommons (a,b,c) = nub (intersect (intersect a b) c)

elfGroups :: [String] -> [(String, String, String)]
elfGroups [] = []
elfGroups (x:y:z:xs) = (x,y,z) : elfGroups xs