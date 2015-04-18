module Chapter4 where

import Data.Char (digitToInt, isDigit)
import Data.Maybe

-- First set of exercises

-- 1.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit xs = Just (safeInit' xs) where
  safeInit' (x:[]) = []
  safeInit' (x:xs) = x : safeInit' xs

-- 2.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p l
  | null l    = []
  | null xs   = splitWith p $ tail xs'
  | otherwise = xs : splitWith p xs' where
    (xs, xs') = break p l

-- 3.
interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (f input)

writeFirstWords inputFile outputFile = interactWith firstWords inputFile outputFile where
  firstWord "" = ""
  firstWord line = head $ words line
  firstWords file = unlines $ map firstWord $ lines file

--4.
transpose = unlines . transpose' . lines where
  transpose' ss | all ((==) "") ss = []
  transpose' ss                    = map head nonEmptySs : (transpose' $ map tail nonEmptySs) where
    nonEmptySs = filter ((/=) "") ss

transposeFile inputFile outputFile = interactWith transpose inputFile outputFile

-- Second set of exercises

-- 1.
asInt' :: String -> Int
asInt' ""       = error "Empty string"
asInt' ('-':[]) = error "String must have digits following -"
asInt' ('-':ds) = negate $ asInt' ds
asInt' (d:ds)   = foldl (\a d -> 10*a + digitToInt' d) 0 (d:ds) where
  digitToInt' d | d `elem` ['0'..'9'] = digitToInt d
  digitToInt' _                       = error "String contains non-digit characters"

-- 2.
type ErrorMessage = String
asInt'' :: String -> Either ErrorMessage Int
asInt'' ""       = Left "Empty string"
asInt'' ('-':[]) = Left "String must have digits following -"
asInt'' ('-':ds) =
  let (Right int) = asInt'' ds in
    Right $ negate int
asInt'' ds
  | not $ all isDigit ds = Left "String contains non-digit characters"
  | otherwise            = Right $ foldl (\a d -> 10*a + digitToInt d) 0 ds where

-- 3.
concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

-- 4.

takeWhileTr :: (a -> Bool) -> [a] -> [a]
takeWhileTr p l = takeWhileTr' l [] where
  takeWhileTr' [] acc     = acc
  takeWhileTr' (x:xs) acc
    | p x       = takeWhileTr' xs (acc ++ [x])
    | otherwise = acc

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold p l = foldr f [] l where
  f x acc
    | p x       = x:acc
    | otherwise = []

-- 5.

groupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold f (h:t) = foldl f' [[h]] t where
  f' acc x
    | f prevX x = iAcc ++ [lAcc ++ [x]]
    | otherwise = acc ++ [[x]] where
      iAcc  = init acc
      lAcc  = last acc
      prevX = head lAcc

-- 6.

anyFold :: (a -> Bool) -> [a] -> Bool
anyFold f = foldr f' False where
  f' x acc
    | f x       = True
    | otherwise = acc

cycleFold :: [a] -> [a]
cycleFold l = foldr f [] [1..] where
  f _ acc = l ++ acc

wordsFold :: String -> [String]
wordsFold = foldr f [] where
  f x acc
    | x `elem` ['\n','\t'] = "":acc
    | otherwise            = (x:w):ws where
      (w:ws) = acc

unlinesFold :: [String] -> String
unlinesFold = foldr (\x acc -> x ++ ['\n'] ++ acc) ""
