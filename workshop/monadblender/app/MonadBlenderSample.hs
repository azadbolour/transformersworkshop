{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- IMPORTANT - Need ghc >= 8.0.1 to get monad instance of ((,), w) used here.

-- See documentation at end of file.

-- TODO. Move documentation to doc comments. Move MonadBlender to its own module.

module Main where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Tuple
import MonadBlender

main :: IO ()

-- type MaybeTExceptT err monad a = MaybeT (ExceptT err monad) a
-- type ExceptTWriterT err w monad a = ExceptT err (WriterT w monad) a

exceptFactory :: Int -> Either String Int
exceptFactory x = Right $ 2*x
maybeFactory :: Int -> Maybe  Int
maybeFactory x = Just $ 100 + x
writerFactory :: Int -> (String, Int)
writerFactory x = ("log", x)

baseMonad = [1, 2]

x1 = (baseMonad `hBind` exceptFactory) `hBind` maybeFactory

f1 = (exceptFactory `hCompose` maybeFactory) `hCompose` writerFactory
-- f2 = exceptFactory `hCompose` (maybeFactory `hCompose` writerFactory) -- No can do! No blender for WriterT!
-- TODO. Can we define the blender for WriterT to be WriterT? Problem - won't have uniqueness leading to ambiguities.
-- Not sure if we can represent this model nicely without functional dependencies.
-- In order to even have a hope of associativity, we need to peel off the shells.
-- Exposing core monads that do have blenders.
-- TODO. Would this work for reader and state that use core functions?

g1 = runMaybeT . runWriterT . ((exceptFactory `hCompose` maybeFactory) `hCompose` writerFactory)
g2 = runMaybeT . (exceptFactory `hCompose` (runWriterT . (maybeFactory `hCompose` writerFactory)))

y1 = f1 1
z1 = g1 1
z2 = g2 1

maybeBoundList :: MaybeT [] Int
maybeBoundList = hBind [1, 2] Just

maybeBoundEither :: MaybeT (Either String) Int
maybeBoundEither = hBind (Right 5) Just

right :: a -> Either String a
right a = Right a

eitherBoundList :: ExceptT String [] Int
eitherBoundList = hBind [1, 2] right

eitherBoundMaybe :: ExceptT String Maybe Int
eitherBoundMaybe = hBind (Just 5) right

writerBoundList :: WriterT String [] Int
writerBoundList = hBind [1, 2] (\i -> ("log", 2 * i))

type MyEnv = Int
readerFactory :: String -> MyEnv -> Int
readerFactory s = \i -> (length s) + i

readerBoundList :: ReaderT MyEnv [] Int
readerBoundList = hBind ["David", "Mary"] readerFactory

type Count = Int
increment :: State' Count Int
increment = State' { runState' = \count -> (count, count + 1) }

stateFactory :: Int -> State' Count Int
stateFactory x = do
   increment
   return (2 * x)

stateBoundList :: StateT Count [] Int
stateBoundList = hBind [1, 2] stateFactory

runStateBoundList :: Count -> [(Int, Count)]
runStateBoundList = runStateT stateBoundList

stateBoundMaybe :: StateT Count Maybe Int
stateBoundMaybe = hBind (Just 100) stateFactory

runStateBoundMaybe :: Count -> Maybe (Int, Count)
runStateBoundMaybe = runStateT stateBoundMaybe

maybeLiftedList :: MaybeT [] Int
maybeLiftedList = hLift [1, 2]

eitherLiftedMaybe :: ExceptT String Maybe Int
eitherLiftedMaybe = hLift (Just 100)

composed :: Int -> MaybeT [] Int
composed = (\i -> [i, i + 1]) `hCompose` (\i -> if i == 0 then Nothing else (Just i))



main = do
    print maybeBoundList
    print maybeBoundEither
    print eitherBoundList
    print eitherBoundMaybe
    print writerBoundList
    print $ runReaderT readerBoundList 100
    print $ runStateBoundList 0
    print $ runStateBoundMaybe 0
    print $ maybeLiftedList
    print $ eitherLiftedMaybe
    print $ composed 0
    print x1
    print y1
    print z1
    print z2


{-

One way to motivate monad transformers is through the ideas of heterogeneous
monad composition and heterogeneous bind.

In analogy to regular bind

>>= :: (m a -> (a -> m b) -> m b)

we can define a heterogeneous bind between two different monads as:

hBind :: baseMonad a -> (a -> BlendingMonad b) -> BlendingMonadT baseMonad b

Maybe there is a standard term for this notion that I am unaware of.

hBind can be used to define heterogeneous composition: the composition of two
monad-producing functions that produce different monads (just as bind can be
used to define Kleisli composition when the monads are the same):

f :: a -> baseMonad b
g :: b -> BlendingMonad c

f `hCompose` g :: a -> BlendingMonadT baseMonad c

Applying f produces a 'baseMonad b' value which can then be subjected to a
heterogeneous bind to produce the result of the composition.

To implement heterogeneous bind we try to mimic the implementation of bind
via join:

monad >>= monadFactory = join $ monadFactory <$> monad

The fmap still makes sense when the monads are different, but the join
would have to be special, call it hJoin.

baseMonad `hBind` blendingFactory = hJoin $ blendingFactory <$> baseMonad

Reducing hBind to its constituent parts, we have:

nestedBlender :: (Monad baseMonad, Monad blendingMonad) =>
    baseMonad b -> (b -> blendingMonad c) -> baseMonad (blendingMonad c)
nestedBlender baseMonad blendingFactory = blendingFactory <$> baseMonad

baseMonad `hBind` blendingFactory = hJoin $ nestedBlender baseMonad blendingFactory

In the case of bind, the join flattens the nested structure. hBind can't necessarily
do any flattening. It would just have to do whatever is necessary to convert the
nested structure to a data structure that naturally represents the effects of both
baseMonad and blendingMonad, and is itself a monad capable of propagating the
two effects together.

This scenario provides a simple motivation both for the need for monad transformers,
and for getting an initial handle on their structure.

In some cases, no further proper simplification of the nested structure is
possible or necessary and the hJoin just houses the nested data structure in a
newtype.

In other cases, further proper processing is necessary. For example, when the
blending monad is a reader: '(->) a', the nested structure needs to be
simplified to 'a -> baseMonad c' to preserve the idea that the combined
structure represents both the effect of the base monad, and the effect of
the reader monad as a function from a.

Following this strategy for defining transformers, one could conceivably
base the notion of a monad transformer on a type class that provides an
hJoin function, and uses it to implement hBind and hCompose.

However, the designers of the transformer library decided to base their
treatment on the idea of lifting. Interestingly, lift can be defined
generically using hBind:

    lift baseMonad = baseMonad `hBind` return

I have not checked the lift laws for this definition yet. But I am guessing that
they would be satisfied.

But can hJoin be defined generically using lift? Not sure.

Is hJoin a strictly more primitive concept than lift? Should we be able
to define hBind generically for all monad-monad transformer pairs based
only on the MonadTrans and Monad abstractions?

-}
