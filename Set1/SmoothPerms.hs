module SmoothPerms where

smooth_perms :: Int -> [Int] -> [[Int]]

data Tree a = Node [Tree a] deriving (Show, Eq, Ord)


