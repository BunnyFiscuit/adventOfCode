module FileReader where

import System.IO

readF :: Int -> IO [String]
readF n = do
  contents <- readFile ("../inputs/" ++ show n ++ ".txt")
  return $ lines contents

readF' :: String -> IO [String]
readF' s = do
  contents <- readFile ("../inputs/" ++ s ++ ".txt")
  return $ lines contents

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'