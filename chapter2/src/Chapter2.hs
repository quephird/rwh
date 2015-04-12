module Chapter2 where

-- 1.

-- This is unsafe because it doesn't properly handle the case
-- where a list has either no or just one element.
lastButOne :: [a] -> a
lastButOne (x : y : []) = x
lastButOne (x : xs) = lastButOne xs
