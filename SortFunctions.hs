module SortFunctions where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lower) ++ [x] ++ (quicksort higher)
    where lower = [y | y <- xs, y <= x]
          higher = [y | y <- xs, y > x]

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x] 
mergesort l = merge (mergesort front) (mergesort back)
    where 
       front = take ((length l) `div` 2) l
       back = drop ((length l) `div` 2) l

       merge [] ys = ys
       merge xs [] = xs
       merge (x:xs) (y:ys)
          | x < y = x:(merge xs (y:ys))
          | otherwise = y:(merge (x:xs) ys)
