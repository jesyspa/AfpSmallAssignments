module CountZero where

-- The solution is based on recursion induced by the structure of the type
-- we'd like count to be of.
--
-- We let Int be the base case of this recursion, defining count = 0.  We
-- then assume count can be of type b, and let count :: a -> b be the
-- constant function that returns count :: b.  This is possible for all a,
-- satisfying the assignment requirements.

class Countable a where
    count :: a

instance Countable Int where
    count = 0

instance Countable b => Countable (a -> b) where
    count = const count

