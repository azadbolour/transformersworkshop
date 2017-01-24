{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- IMPORTANT - Need ghc >= 8.0.1 to get monad instance of ((,), w) used here.

-- See documentation at end of file.

-- TODO. Move documentation to doc comments. Move MonadBlender to its own module.

module MonadBlender (
    MonadBlender (..)
  , hBind
  , hLift
  , hCompose
  , State' (..)
  ) where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Tuple

{- |
    One way to motivate monad transformers is through the ideas of heterogeneous
     bind and heterogeneous monad composition.

    In analogy to regular bind:

    @ '>>= :: (m a -> (a -> m b) -> m b)' @

    we can define a heterogeneous bind between two different monads as:

    @ 'hBind :: baseMonad a -> (a -> blendingMonad b) -> blender baseMonad b' @

    MonadBlender is a binary type class that relates /monad blenders/ to
    corresponding /blending monads/, e.g., a maybe blender, to Maybe.
    The blending monad is primitive: it is defined independently of the blender.
    Examples of primitive monads are Maybe, ((->) a), ((,) a), Either, etc.

    As a data structure, a monad blender is identical to the corresponding
    transformer, and represents two distinct effects: the effect of the
    corresponding blending monad (Maybe in the above example), and the effect of
    some other (generic) monad, called a /base monad/ here.

    But as a type class, /MonadBlender/ is based on the idea of
    /heterogeneous bind/ (/hBind/), rather than on the idea of /lift/ used
    in the /MonadTrans/ type class.

    In a heterogeneous bind, the type of the monad produced by the binding
    function is known and fixed, but the type of the monad being bound (the
    base monad) is unknown (generic).

    MonadTrans's lift method can be represented generically in terms of
    heterogeneous bind. TODO. The converse remains to be proved or disproved.

    Heterogeneous bind may in turn be used to define the /heterogeneous/
    /composition/ (/hCompose/) of two monad-producing functions producing
    different types of monads, just as normal bind can be used to define
    Kleisli composition (>=>).

    Heterogeneous bind itself is implemented by using a /heterogeneous join/
    (/hJoin/) function, in the same way that homogeneous bind (>>=)
    can be implemented by using monadic join. Hence, /hJoin/ is the only
    method of the monad blender type class. Other methods
    related to this type class, such as /hBind/, are stand-alone methods,
    since their implementations in terms of /hJoin/ are fixed.

    Technical note.

    Certain functions related to this type class, e.g., /hLift/
    (the implementation of MonadTrans's /lift/), only use the
    base monad and the blender in their signatures, but need the blending
    monad in their implementations. To avoid ambiguity in the choice
    the blending monad in these implementations, a functional dependency
    is added between the blender and the blending monad.

    TODO. Can remove the FP by using a proxy parameter for the blending monad.
    But this is not necessary for our current pedagogical purposes.

-}

class (Monad blendingMonad) => MonadBlender blender blendingMonad | blender -> blendingMonad, blendingMonad -> blender where
    -- | Heterogeneous join: convert a nested data structure: baseMonad (blendingMonad a)
    -- into a corresponding blender data structure. /hJoin/ is used to implement /hBind/:
    --
    -- @ baseMonad ``hBind`` blendingMonadFactory = hJoin $ blendingMonadFactory '<$>' baseMonad @
    --
    -- Analogous to the implementation of bind via join:
    --
    -- @ monad '>>=' monadFactory = join $ monadFactory '<$>' monad @
    --
    -- In the case of bind, /join/ flattens the nested structure. /hBind/ can't necessarily
    -- do any flattening. It would just have to do whatever is necessary to convert the
    -- nested structure to a data structure that naturally represents the effects of both
    -- baseMonad and blendingMonad, and is itself a monad, capable of propagating the
    -- two effects together.
    --
    -- In some cases, no further proper simplification of the nested structure is
    -- possible or necessary and hJoin just houses the nested data structure in a
    -- newtype.
    --
    -- In other cases, further proper processing is necessary. For example, when the
    -- blending monad is a reader: '(->) r', the nested structure needs to be
    -- simplified to 'r -> baseMonad a' to preserve the idea that the combined
    -- structure represents both the effect of the base monad, and the effect of
    -- the reader monad as a function from r.
    hJoin :: (Monad baseMonad) => baseMonad (blendingMonad a) -> blender baseMonad a

-- | Map a blending monad-producing function onto a base monad.
--   The slots of the base monad are replaced by blending monads
--   in a nested arrangement. Used in the implementation of hBind.
nestedBlender :: (Monad baseMonad, Monad blendingMonad) =>
    baseMonad a -> (a -> blendingMonad b) -> baseMonad(blendingMonad b)
nestedBlender baseMonad blendingMonadFactory = blendingMonadFactory <$> baseMonad

-- | Heterogeneous bind: bind a base monad by using a blending monad producer.
--   Return a blender corresponding to the blending monad that represents
--   both the effect of the base monad, and the effect of the blending monad.
-- hBind :: (Monad baseMonad, Monad blendingMonad, MonadBlender blender blendingMonad) =>
hBind :: (Monad baseMonad, MonadBlender blender blendingMonad) =>
    baseMonad a -> (a -> blendingMonad b) -> blender baseMonad b
hBind baseMonad blendingMonadFactory = hJoin $ nestedBlender baseMonad blendingMonadFactory

-- | Implementation of /lift/ by using heterogeneous bind.
hLift :: (Monad baseMonad, MonadBlender blender blendingMonad) =>
    baseMonad a -> blender baseMonad a
hLift baseMonad = hBind baseMonad return

-- | Heterogeneous Kleisli composition - compose two monad producers
--   producing monads of different types. Analogous to the implementation
--   of Kleisli composition by using homogeneous bind.
hCompose :: (Monad baseMonad, MonadBlender blender blendingMonad) =>
    (a -> baseMonad b) -> (b -> blendingMonad c) -> (a -> blender baseMonad c)
hCompose baseFactory blendingFactory a = hBind (baseFactory a) blendingFactory

-- | Allow any monad to be bound by a Maybe-producing function.
--  The bind produces a MaybeT data structure.
instance MonadBlender MaybeT Maybe where
    hJoin nested = MaybeT nested

-- | Allow any monad to be bound by an Either-producing function.
--  The bind produces an ExceptT data structure.
instance MonadBlender (ExceptT err) (Either err) where
    hJoin nested = ExceptT nested

-- newtype WriterT w m a = WriterT { runWriterT :: m (a, w) } - here w is the second element.
-- The type parameters are swapped from ((,), w) - here w is the first element.

-- | Allow any monad to be bound by a writer- [that is, ((,), w)] producing function.
--  The bind produces a WriterT data structure.
instance (Monoid w) => MonadBlender (WriterT w) ((,) w) where
    hJoin nested = WriterT (swap <$> nested)

-- | Allow any monad to be bound by a reader- [that is, ((->), r)] producing function.
--  The bind produces a ReaderT data structure.
instance MonadBlender (ReaderT r) ((->) r) where
    hJoin nested = ReaderT $ \r -> nested <*> (return r)

-- TODO. Can we define instances for the core of state (s -> (a, s)) w.o. encapsulating it?

-- | Encapsulation of a stand-alone state monad.
newtype State' s a = State' { runState' :: s -> (a, s) }

-- | fmap for the core of state 's -> (a, s)'.
stateMapper :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
stateMapper f runState = \s -> let (a, s') = runState s in (f a, s')

-- | Apply '<*>' for the core of state 's -> (a, s)'.
stateApply :: (s -> (a -> b, s)) -> (s -> (a, s)) -> (s -> (b, s))
stateApply runStateFunction runStateArg =
    \s -> let (function, s') = runStateFunction s
              (arg, s'') = runStateArg s
          in (function arg, s'')

instance Functor (State' s) where
    fmap f (State' runState) = State' $ stateMapper f runState

instance Applicative (State' s) where
    pure a = State' $ \s -> (a, s)
    (State' runStateFunction) <*> (State' runStateArg) =
        State' $ stateApply runStateFunction runStateArg

instance Monad (State' s) where
    return x = State' $ \s -> (x,s)
    (State' runStateA) >>= stateFactory =
        State' $ \s ->
            let (a, s') = runStateA s
                (State' runStateB) = stateFactory a
            in runStateB s'

-- | Allow any monad to be bound by a State'-producing function.
--  The bind produces a StateT data structure.
instance MonadBlender (StateT s) (State' s) where
    hJoin nested = StateT $ \s -> (runState' <$> nested) <*> (return s)



