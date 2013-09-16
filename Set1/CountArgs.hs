module CountArgs where

-- We build upon the previous definition.  We add a helper function that
-- allows us to transform count.  At every step of the recursion, apart
-- from ignoring the argument, we also apply it to count to increment its
-- value.
--
-- The base case implementation does the incrementing.  The a -> b case
-- simply forwards the (still useless) parameter on to make the program
-- type-correct.
--
-- This is actually a specialisation of a more useful typeclass where cmap
-- has type (Int -> Int) -> a -> a.  If in the a -> b case a is restricted
-- sufficiently to make a meaningful Int -> Int function from it, the
-- arguments could be used to determine the value of count.

class Countable a where
    count :: a
    cmap :: a -> a

instance Countable Int where
    count = 0
    cmap = (+1)

instance Countable b => Countable (a -> b) where
    count = const $ cmap count
    cmap x = \a -> cmap (x a)

