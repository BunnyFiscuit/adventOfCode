module Day12 where
import FileReader
import Data.List

alphs = ['a'..'z']

main :: IO ()
main = do
  contents <- readF' "12"
  print alphs
  putStrLn $ show contents



