module Day where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "test"
  putStrLn $ show contents