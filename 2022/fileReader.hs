module FileReader where

import System.IO

readF :: Int -> IO [String]
readF n = do
  contents <- readFile ("inputs/" ++ show n ++ ".txt")
  return $ lines contents

readFStr :: String -> IO [String]
readFStr s = do
  contents <- readFile ("inputs/" ++ s ++ ".txt")
  return $ lines contents