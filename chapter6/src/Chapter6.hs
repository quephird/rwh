module Chapter6 where

-- 1.

-- second takes a function as the first argument and a tuple and
-- returns a tuple consisting of the first of the original tuple
-- and the function applied to the second argument. It is not
-- exactly clear to me what the Arrow typeclass entails but at a
-- minimum it is a function that takes one argument.

-- 2.

-- (,) turns two arguments into a tuple. In fact, there appear to be
-- variations on this function for up at least 40 args (!!!). What is
-- not apparent to me is if these functions pre-exist in Control.Arrow,
-- as not even (,) appears in the documentation on haskell.org,
-- or if there is some sort of dynamicism at work here.
