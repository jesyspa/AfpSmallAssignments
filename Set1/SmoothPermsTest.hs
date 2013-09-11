module Main where

import SmoothPerms
import Test.QuickCheck

fact :: Int -> Int
fact n = product [1..n]

-- In my humble opinion, this shouldn't work, and the fact the concat makes it
-- work is mildly scary.
allPerms :: [a] -> [[a]]
allPerms [] = [[]]
allPerms xs = concat $ foreachWithRest (\y ys -> map (y:) $ allPerms ys) xs

-- We could generalise these to Eq a => [a] -> Bool, but then we'd have to
-- specify that in main.
checkForeachWithRest :: [Int] -> Bool
checkForeachWithRest xs = all f $ foreachWithRest (,) xs
    where f (x, y) = x `elem` xs && 1 + length y == length xs

-- We can't check all lists; takes too long.
checkAllPermsLength :: [Int] -> Property
checkAllPermsLength xs = length xs < 10 ==> length perms == fact (length xs) && xs `elem` perms
    where perms = allPerms xs

main = quickCheck (checkForeachWithRest .&&. checkAllPermsLength)
