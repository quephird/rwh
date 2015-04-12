module Chapter1 where

-- 1.
main = interact printWordCount where
  printWordCount input = (show $ length $ words input) ++ "\n"
