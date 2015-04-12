module Chapter3 where

import Data.List (sortBy)

-- Definitions from narrative

data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- First set of exercises

-- 1.
fromList :: [a] -> List a
fromList (x:xs) = Cons x $ fromList xs
fromList [] = Nil

toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = []

--2.
data MaybeTree a = MaybeNode (Maybe (a, MaybeTree a, MaybeTree a))
                   deriving (Show)

-- Second set of exercises

-- 1.
myLength :: Num b => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 3.
mean :: Fractional a => [a] -> a
mean xs = sum / count where
  sum = foldl (+) 0 xs
  count = fromIntegral $ length xs

-- 4.
palindrome :: [a] -> [a]
palindrome l = l ++ reverse l

-- 5.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

-- 6.
sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (\xs xs' -> compare xs xs')

-- 7.
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [xs] = xs
intersperse s (xs:xss) = xs ++ [s] ++ intersperse s xss

-- 8.
height :: (Num a, Ord a) => Tree b -> a
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- 9.
data Direction = LeftTurn | Straight | RightTurn
                 deriving (Show)

-- 10.
computeTurn :: (Floating a, Ord a) => (a,a) -> (a,a) -> (a,a) -> Direction
computeTurn (x1,y1) (x2,y2) (x3,y3)
  | crossProductZ > 0  = LeftTurn
  | crossProductZ == 0 = Straight
  | otherwise  = RightTurn where
    crossProductZ = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

-- 11.
computeTurns :: (Floating a, Ord a) => [(a,a)] -> [Direction]
computeTurns (p1:p2:p3:[]) = [computeTurn p1 p2 p3]
computeTurns (p1:p2:p3:ps) = (computeTurn p1 p2 p3) : computeTurns (p2:p3:ps)
computeTurns _             = []
