module CountZero where

class Countable a where
    count :: a

instance Countable Int where
    count = 0

instance Countable b => Countable (a -> b) where
    count = const count

