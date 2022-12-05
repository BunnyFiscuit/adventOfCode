module FileReader where

import System.IO

readF :: Int -> IO [String]
readF n = do
  contents <- readFile ("inputs/" ++ show n ++ ".txt")
  return $ lines contents

readF' :: String -> IO [String]
readF' s = do
  contents <- readFile ("inputs/" ++ s ++ ".txt")
  return $ lines contents