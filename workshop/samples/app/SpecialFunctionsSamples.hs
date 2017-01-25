module Main where

import Data.Char
import Data.Functor.Identity
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

main :: IO ()

doWriterT :: (Monad monad) => Int -> WriterT String monad String

doWriterT x = do
    tell "something"
    computed <- return (show (2 * x))
    (_, w) <- listen $ tell " else" -- Get the appended value.
    tell $ toUpper <$> w
    return $ computed

doReaderT1 :: (Monad monad) => Int -> ReaderT Int monad Int

doReaderT1 x = do
    ask
    y <- local (*5) $ doReaderT2 x -- local setter
    ask

doReaderT2 :: (Monad monad) => Int -> ReaderT Int monad Int
doReaderT2 x = do
    e <- ask
    return $ 8 * x + e

doStateT :: (Monad monad) => Int -> StateT Int monad Int
doStateT x = do
    s <- get
    put (s + x)
    return (2 * x)

main = do
    print $ (runIdentity . runWriterT) $ doWriterT 1
    print $ snd $ (runIdentity . runWriterT) $ listen $ doWriterT 1
    print $ runIdentity (runReaderT (doReaderT1 1) 100)
    print $ runIdentity (runReaderT (doReaderT2 3) 10)
    print $ runIdentity (runStateT (doStateT 200) 1000)



