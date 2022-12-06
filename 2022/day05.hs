module Day5 where
import FileReader
import Data.List
import Data.Maybe
import Stack

main :: IO ()
main = do
  contents <- readF' "test"
  let spl  = break (=="") contents
  let ss   = reverse $ tail (reverse (fst spl))
  let ms   = map toMovement (drop 1 (snd spl))
  let ts   = map toStack (transpose (map (sanStack . rmBracks) ss))
  putStrLn $ "part 1: " ++ show (getPeeks (run ms ts))
  putStrLn $ "part 2: " ++ show (getPeeks (run' ms ts))

run :: [Move] -> [MStack] -> [MStack]
run [] ss     = ss
run (m:ms) ss = run ms (move m ss)

run' :: [Move] -> [MStack] -> [MStack]
run' [] ss     = ss
run' (m:ms) ss = run' ms (move' m ss)

getPeeks xs = concat $ map (fromJust . peek) xs

sanStack :: String -> [String]
sanStack [] = []
sanStack xs = take 1 xs : sanStack xs'
  where xs' = drop 2 xs

rmBracks :: String -> String
rmBracks [] = []
rmBracks (' ':' ':xs) = ' ' : rmBracks xs
rmBracks (x:xs) 
  | x == '[' || x == ']' = rmBracks xs
  | otherwise  = x : rmBracks xs

-- M Amount FromStack ToStack
data Move = M Int Int Int deriving (Show, Eq)

type MStack = Stack String

toMovement :: String -> Move
toMovement ss = M (read (ws !! 1)) (read (ws !! 3)) (read (ws !! 5))
  where ws = words ss

toStack :: [String] -> MStack
toStack xs = foldl push (empty :: MStack) (reverse (words (unwords xs)))

move :: Move -> [MStack] -> [MStack]
move (M 0 _ _)     xs = xs
move (M amt s1 s2) xs = move (M (amt-1) s1 s2) xs'
  where
    s1' = pop (xs !! (s1-1)) 
    s2' = case s1' of
          Just (s1', v) -> push (xs !! (s2-1)) v
          Nothing -> error "No pop"
    xs' = replace (s2-1) s2' (replace (s1-1) (fst (fromJust s1')) xs)

move' :: Move -> [MStack] -> [MStack]
move' (M 0 _ _)   xs   = xs
move' m@(M 1 _ _) xs   = move m xs
move' (M amt s1 s2) xs = xs'
  where
    (s1', temp)  = popX (xs !! (s1-1), empty :: MStack) amt
    (temp', s2') = pushX (temp, xs !! (s2-1))
    xs' = replace (s2-1) s2' (replace (s1-1) s1' xs)

popX :: (MStack, MStack) -> Int -> (MStack, MStack)
popX t@(from, to) 0 = t
popX t@(from, to) n = popX (from', push to v) (n-1)
  where (from', v) = case pop from of
                   Just (s, v) -> (s, v)
                   Nothing     -> error "popX: No pop"

pushX :: (MStack, MStack) -> (MStack, MStack)
pushX (temp, to)
  | isEmpty temp = (temp, to)
  | otherwise         = case pop temp of
                      Just (temp', v) -> pushX (temp', push to v)
                      Nothing         -> error "pushX: No pop"

replace :: Int -> a -> [a] -> [a]
replace i x xs
  | (length xs - 1) < i = error "replace - index out of bounds"
  | otherwise = a ++ (x : (drop 1 b))
  where (a,b) = splitAt i xs