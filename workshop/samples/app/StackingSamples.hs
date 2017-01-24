module Main where

import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

infixr 9 ...

type WriterStateStack log state monad substrate = WriterT log (StateT state monad) substrate
writerOfState :: (Monad monad) => WriterStateStack String String monad String

writerOfState = do
    tell "log info"
    return "substrate 1"

{- |
  Sample runner (core accessor) for a stack containing state - WriterStateStack.

  The relevant type:

  @ runWriterT :: WriterT log monad substrate -> monad (substrate, log) @
  @ runStateT :: StateT state monad substrate -> state -> monad (substrate, state) @
  @ runStateT . runWriterT :: WriterT log (StateT state monad) substrate ->
       state -> monad (substrate, log) @

  Because the state runner is a function, the composed runner becomes
  a 2-argument functions :: stack -> state -> nested core monad.

  For single-argument functions :: stack -> nested core monad, we can
  frame the function by composing with runIdentity to get a
  function from the stack to the core.

  To frame a 2-argument runner, we need a composition operator that composes
  a 2-argument function with the single-argument function runIdentity. That
  composition operator is defined here as (...). The combined core accessor is a
  2-argument function from stack and state to the core.
-}
runWriterStateStack :: WriterStateStack log state Identity substrate -> state -> ((substrate, log), state)
runWriterStateStack = runIdentity ... runStateT . runWriterT

type WriterReaderStack log env monad substrate = WriterT log (ReaderT env monad) substrate
runWriterReaderStack :: WriterReaderStack log env Identity substrate -> env -> (substrate, log)
runWriterReaderStack = runIdentity ... runReaderT . runWriterT

writerOfReader :: (Monad monad) => WriterReaderStack String String monad String
writerOfReader = do
    tell "log info"
    return "substrate 1"

type WriterReaderStateStack log env state monad substrate = WriterT log (ReaderT env (StateT state monad)) substrate
{-
  Sample runner (core accessor) for a stack containing both reader and state.

  @ runReaderT . runWriterT :: Stack -> env -> StateT @
  @ runStateT :: StateT -> state -> monad (substrate, state) = StateT -> (state -> monad (substrate, state)) @

  ...-composition of runReaderT . runWriterT with runStateT joins on StateT and yields
  a 2-argument function Stack -> env -> (state -> monad (substrate, state))
  That is a 3-arg function :: Stack -> env -> state -> monad (substrate, state)
-}
runWriterReaderStateStack :: WriterReaderStateStack log env state monad substrate -> env -> state -> monad ((substrate, log), state)
runWriterReaderStateStack = runStateT ... runReaderT . runWriterT

writerOfReaderOfState :: (Monad monad) => WriterReaderStateStack String String String monad String
writerOfReaderOfState = do
    tell "log info"
    return "substrate 1"

type MaybeWriterReaderStack log env monad substrate = MaybeT (WriterT log (ReaderT env monad)) substrate
runMaybeWriterReaderStack :: MaybeT (WriterT log (ReaderT env monad)) substrate -> env -> monad (Maybe substrate, log)
runMaybeWriterReaderStack = runReaderT . runWriterT . runMaybeT

type MaybeReaderWriterStack env log monad substrate = MaybeT (ReaderT env (WriterT log monad)) substrate
runMaybeReaderWriterStack :: MaybeT (ReaderT env (WriterT log monad)) substrate -> env -> monad (Maybe substrate, log)
runMaybeReaderWriterStack = runWriterT ... runReaderT . runMaybeT

main :: IO ()
main = do
    print $ runWriterStateStack writerOfState "state 1"
    print $ runWriterReaderStack writerOfReader "my env"
    print $ runIdentity $ runWriterReaderStateStack writerOfReaderOfState "my env" "state 1"
    print $ runIdentity $ runWriterReaderStateStack writerOfReaderOfState "my env" "state 1"


{-
  Composition of a 2-argument function fabx with a single-argument function fxc.
  Applies fabx to 2-arguments, and applies fxc to the result, yielding a
  2-argument function fabc.
-}
(...) :: (x -> c) -> (a -> b -> x) -> (a -> b -> c)
(fxc ... fabx) a b = fxc (fabx a b)
-- (...) = ((.) . (.)) -- The equivalent cute form that motivates the notation.

{-
    (...) fxc fabx a b = fxc ((fabx a) b) = (fxc . (fabx a)) b -- b is redundant:
    (...) fxc fabx a = fxc . (fabx a) = (.) fxc (fabx a)
      = ((.) fxc) (fabx a) = ((.) fxc) . fabx) a -- a is redundant
    (...) fxc fabx = (fxc ... fabx) = ((.) fxc) . fabx = (.) ((.) fxc) fabx -- fabx is redundant:
    (...) fxc = (.) ((.) fxc) = ((.) . (.)) fxc -- Voila!
-}
