module CountArgs where

class Countable a where
    count :: a
    cmap :: (Int -> Int) -> a -> a

instance Countable Int where
    count = 0
    cmap = id

instance Countable b => Countable (a -> b) where
    count = const (cmap (+1) count)
    cmap f x = \a -> cmap f (x a)

