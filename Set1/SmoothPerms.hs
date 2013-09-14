module SmoothPerms where

import Data.Maybe (isJust)

--smooth_perms :: Int -> [Int] -> [[Int]]

data Tree a = Node a [Tree a] deriving (Show, Eq, Ord)

-- unfoldTree :: (b -> [(a, b)]) -> b -> Tree a
-- unfoldTree f b = Node . map g $ f b
--     where g (x, y) = (x, unfoldTree f y)

-- | Given a function, apply it to every element of the list, passing along the
-- list excluding that element.
--
-- Does not preserve order, but we don't care.
--
-- There has got to be a better name for this.
-- foreachWithRest :: (a -> [a] -> b) -> [a] -> [b]
-- foreachWithRest f = go []
--     where go ys [] = []
--           go ys (x:xs) = f x (ys ++ xs) : go (x:ys) xs

-- buildPermTree :: [a] -> Tree a
-- buildPermTree = unfoldTree (foreachWithRest (,))

-- | Remove the first element in the list that suffices
-- the given contition
filterFst :: (a -> Bool) -> [a] -> [a]
filterFst _ [] = []
filterFst eq (x:xs)
  | eq x = xs
  | otherwise = x:filterFst eq xs

-- | Given a previous element and the maximum allowed distance,
-- this function creates a gree that when traversed in DFS mode
-- can be concatenated to obtain all smooth permutations with
-- respect to k
smooth_perms' :: (Num a,Eq a,Ord a) => a -> a -> [a] -> Maybe (Tree a)
smooth_perms' _ prev [] = Just $ Node prev []
smooth_perms' k prev xs =
  next >>= \rest -> return $ Node prev rest
  where
    next = case filter isJust $ concatMap continueSmooth xs of
      [] -> Nothing
      xs' -> sequence xs'
    continueSmooth x
      | abs (prev-x) <= k = [smooth_perms' k x $ filterFst (x==) xs]
      | otherwise = []

-- | Traverse the tree and concatenate all the paths from root
-- to leave in a list to form a path
flatten :: Tree a -> [[a]]
flatten (Node p []) = [[p]]
flatten (Node p ts) =
  map (p:) $ concatMap flatten ts

-- This function uses several trees, one for every element
-- of the list. The reason is that there are no transitions
-- from one smooth permutation to the next that require swapping
-- only two letters. In fact the number of letter one needs to
-- swap depends on the input list and the given Int
-- | Generate all the smooth permutations with respect
-- to the provided 'Int' that exist in the given list
smooth_perms :: (Num a,Eq a,Ord a) => a -> [a] -> [[a]]
smooth_perms _ [] = [[]]
smooth_perms k xs = concatMap flatten $ pTrees >>= getJust
  where
    getJust Nothing = []
    getJust (Just x) = [x]
    pTrees = map (\x-> smooth_perms' k x $ filterFst (x==) xs) xs
        
