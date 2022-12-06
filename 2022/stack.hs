module Stack ( 
  Stack,
  empty,
  push,
  pop,
  peek,
  isEmpty,
  size,
  toList,
  fromList
) where

import Numeric.Natural

-- | Stack data type
data Stack a = Stack !Natural [a] deriving (Read, Show)

empty :: Stack a
empty = Stack 0 []

push :: Stack a -> a -> Stack a
push (Stack sz xs) x = Stack (succ sz) (x:xs)

peek :: Stack a -> Maybe a
peek (Stack _ []) = Nothing
peek (Stack _ (x:xs)) = Just x

pop :: Stack a -> Maybe (Stack a, a)
pop (Stack _ []) = Nothing
pop (Stack sz (x:xs)) = Just (Stack (pred sz) xs, x)

isEmpty :: Stack a -> Bool
isEmpty (Stack _ []) = True
isEmpty (Stack _ _ ) = False

size :: Stack a -> Natural
size (Stack sz _) = sz

toList :: Stack a -> [a]
toList (Stack _ xs) = xs

fromList :: [a] -> Stack a
fromList = foldl push empty . reverse
