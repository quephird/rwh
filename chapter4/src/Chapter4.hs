module Chapter4 where

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

transposeFile inputFile outputFile = interactWith transpose inputFile outputFile where
