
-- module ProfilingEvaluator where

import Control.Monad.Identity
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

{-
  Evaluator that has the environment factored out and profiles evaluation operations.
-}

-- See README.md for signatures and descriptions of the transformers.

type OperationCount = Integer

-- The value we are interested is the expression value augmented with
-- the number of operations executed to evaluate the expression.
type TransformerStack monad val = ReaderT Env (ExceptT String (StateT OperationCount monad)) val

runTransformerStack :: Env -> OperationCount -> TransformerStack Identity val -> (Either String val, OperationCount)
runTransformerStack env = \initialCount effectfulVal -> runIdentity(runStateT(runExceptT (runReaderT effectfulVal env)) initialCount)

tick :: (Num count, MonadState count stateMonad) => stateMonad ()
tick = do
  count <- get
  put (count + 1)

eval :: (Monad monad) => Exp -> TransformerStack monad Value

eval (Lit i) = do
  tick
  return $ IntVal i

eval (Var var) = do
  tick
  env <- ask
  case Map.lookup var env of
    Nothing -> throwError ("unbound variable: " ++ var)
    Just val -> return val

eval (Plus expr1 expr2) = do
  tick
  val1 <- eval expr1
  val2 <- eval expr2
  case (val1, val2) of
    (IntVal i1, IntVal i2) ->
      return $ IntVal (i1 + i2)
    _ -> throwError "type error"

eval (Lambda var expr) = do
  tick
  env <- ask
  return $ FunVal env var expr

eval (Apply expr1 expr2) = do
  tick
  fun <- eval expr1
  val <- eval expr2
  case fun of
    FunVal closureEnv name body ->
      local(const(Map.insert name val closureEnv)) (eval body)
    _ -> throwError "type error"

exampleExp = Lit 12 `Plus` (Apply (Lambda "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (Apply (Lambda "x" (Var "y")) (Lit 4 `Plus` Lit 2))


main :: IO ()

main = do
  let initialCount = 0
  print $ runTransformerStack Map.empty initialCount (eval exampleExp)
  print $ runTransformerStack Map.empty initialCount (eval exampleExpBad)
  print $ runTransformerStack Map.empty initialCount (eval (Plus (Lit 1) (Lambda "x" (Var "x"))))

  -- TODO. How can I force evaluation inside the identity monad but get the monadic value and print it out?

