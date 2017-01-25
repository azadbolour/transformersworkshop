module Main where

{-
   Heterogeneous composition of a list factory with a function factory.
   Illustrating the individual steps of heterogeneous composition when
   the blending monad is a function (primitive reader monad).
-}
type A = Int
type B = Int
type C = Int

type Env = Int

type BaseMonad a = [a]
type BlendingMonad a = (->) a

num :: Int
num = 6

-- The first factory produces lists.
baseFactory :: A -> BaseMonad B
baseFactory a = [0 .. a]

-- The first step of the composition is to just apply the first factory.
baseMonadB :: BaseMonad B
baseMonadB = baseFactory num

-- The second monad is the primitive reader monad, i.e., a function.
-- So our second monad factory is a "function factory".
blendingFactory :: B -> (Env -> C)
blendingFactory b = ((+) b)

-- The default combination rule is to nest the blending monad inside the base monad.
type Nested = BaseMonad (Env -> C)

-- The basic blending strategy is to map the blending factory onto the base monad.
nestedFunctions :: Nested
nestedFunctions = blendingFactory <$> baseMonadB

-- The nested type carries the idea of a list but does not directly carry the idea of
-- a function that takes a single a-value and returns something.
--
-- To carry that idea we need to apply each of the function slots of teh base monad
-- to the same a-value. That a-value will 'flatten' the function slots by causing
-- the function to be applied to it.

-- Convert a set of nested functions to a function of nested values.
flattenedLambda :: Env -> BaseMonad C
flattenedLambda a = nestedFunctions <*> (return a)

-- This flattened data structure forms the core of our result.
-- Ignoring that the core is housed in a newtype data structure we'd get:
readerHJoin :: Nested -> Env -> BaseMonad C
readerHJoin monadOfFunctions = flattenedLambda

main :: IO ()

main = do
    print $ flattenedLambda 2
    print $ flattenedLambda 10
