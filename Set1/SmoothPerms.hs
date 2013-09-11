module SmoothPerms where

--smooth_perms :: Int -> [Int] -> [[Int]]

data Tree a = Node [(a, Tree a)] deriving (Show, Eq, Ord)

unfoldTree :: (b -> [(a, b)]) -> b -> Tree a
unfoldTree f b = Node . map g $ f b
    where g (x, y) = (x, unfoldTree f y)

-- | Given a function, apply it to every element of the list, passing along the
-- list excluding that element.
--
-- Does not preserve order, but we don't care.
--
-- There has got to be a better name for this.
foreachWithRest :: (a -> [a] -> b) -> [a] -> [b]
foreachWithRest f = go []
    where go ys [] = []
          go ys (x:xs) = f x (ys ++ xs) : go (x:ys) xs

buildPermTree :: [a] -> Tree a
buildPermTree = unfoldTree (foreachWithRest (,))
