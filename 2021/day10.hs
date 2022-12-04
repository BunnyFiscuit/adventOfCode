module Day10 where
import FileReader
import Data.List
import Debug.Trace

main :: IO ()
main = do 
  ls <- readF 10
  -- putStrLn $ show ls
  cs <- mapM (runFirst []) ls
  -- putStrLn $ show cs ++ "\n"
  let fs = filter (/='n') cs
  putStrLn $ "part 1: " ++ show (sum (map points fs))
  let zs = zip cs ls
  let fs' = filter ((=='n') . fst) zs
  let ls' = map snd fs'
  -- putStrLn $ show (length ls')
  cs' <- mapM (runSecond []) ls'
  let os = map (map other) cs'
  -- putStrLn $ show os
  let index = div (length cs') 2
  let ms = map (part2Calc 0) os
  putStrLn $ "part 2: " ++ show ((sort ms) !! index)

closer = ")]}>"
opener = "<{[("

runFirst :: String -> String -> IO Char
runFirst st [] = return 'n'
runFirst st (c:cs) = do
  case isCloser c of
    False -> runFirst (c:st) cs
    True  -> do
      let t = top st
      case other t == c of
        True  -> runFirst (pop st) cs
        False -> return c
        
runSecond :: String -> String -> IO String
runSecond st [] = return st
runSecond st (c:cs) = do
  case isCloser c of
    False -> runSecond (c:st) cs
    True  -> do
      let t = top st
      case other t == c of
        True  -> runSecond (pop st) cs
        False -> runSecond st cs

isCloser :: Char -> Bool
isCloser c = elem c closer
  
pop :: String -> String
pop []     = []
pop (x:xs) = xs

top :: String -> Char
top [] = 'T'
top (x:xs) = x

other :: Char -> Char
other '[' = ']'
other '(' = ')'
other '<' = '>'
other '{' = '}'
other  _  = '0'

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137

points2 :: Char -> Int
points2 ')' = 1
points2 ']' = 2
points2 '}' = 3
points2 '>' = 4

part2Calc :: Int -> String -> Int
part2Calc n [] = n
part2Calc n (x:xs) = part2Calc ((5 * n) + points2 x) xs