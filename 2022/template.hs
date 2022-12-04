-- Change Module Name
module Day where
import FileReader
import Data.List

-- Change file number
main :: IO ()
main = do
  contents <- readF 0
  putStrLn $ show contents