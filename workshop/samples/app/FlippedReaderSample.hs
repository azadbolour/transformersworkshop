module Main where

import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

type Env = String
type StackT result = ReaderT Env (MaybeT IO) result

runFlipped :: Env -> StackT result -> IO (Maybe result)
runFlipped env stack =  runMaybeT $ (runReaderT stack) env

action1 :: String -> StackT String
action2 :: String -> StackT String

action1 s = ReaderT (\env -> MaybeT $ return $ Just $ env ++ s)
action2 s = ReaderT ( MaybeT . return . Just . (++ s) )

main :: IO ()
main = do
    putStrLn "enter a string env value for reader"
    env <- getLine
    result <- runFlipped env $ do
        s1 <- action1 "1"
        s2 <- action2 "2"
        return (s1 ++ s2)
    print result
