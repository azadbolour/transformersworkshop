
-- module GracefulEvaluator where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Maybe (Maybe, fromJust)
import qualified Data.Map as Map (lookup, empty, insert)
import EvaluatorTypes (VarName, Exp(Lit, Var, Plus, Lambda, Apply), Value(IntVal, FunVal), Env)
import EvaluatorTypes (returnValueOrFail)

{-
  Evaluator that encapsulates errors in the Left alternative of an Either.
-}

type TransformerStack monad val = ExceptT String monad val

eval :: Monad monad => Env -> Exp -> TransformerStack monad Value

eval env (Lit i) = return $ IntVal i

eval env (Var var) = do
  let maybeValue = Map.lookup var env
  case maybeValue of
    Just val -> return val
    _ -> throwE $ "undefined variable: " ++ var

-- throwError returns a monad that encapsulates an Either Left.

eval env (Plus expr1 expr2) = do
  val1 <- eval env expr1
  val2 <- eval env expr2
  case (val1, val2) of
    (IntVal i1, IntVal i2) ->
      return $ IntVal (i1 + i2)
    _ -> throwE "type error"

eval env (Lambda var expr) = return $ FunVal env var expr

eval env (Apply expr1 expr2) = do
  fun <- eval env expr1
  val <- eval env expr2
  case fun of
    FunVal closureEnv name body ->
      eval (Map.insert name val closureEnv) body
    _ -> throwE "type error"

exampleExp = Lit 12 `Plus` (Apply (Lambda "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (Apply (Lambda "x" (Var "y")) (Lit 4 `Plus` Lit 2))

runTransformerStack :: TransformerStack Identity val -> Either String val
runTransformerStack = runIdentity . runExceptT

runJust :: TransformerStack Maybe val -> Either String val
runJust = fromJust . runExceptT

runHead :: TransformerStack [] val -> Either String val
runHead = (!! 0) . runExceptT

runInList :: TransformerStack [] val -> [Either String val]
runInList = (id::[a] -> [a]) . runExceptT

main :: IO ()

main = do
  print $ runTransformerStack $ eval Map.empty exampleExp
  print $ runJust $ eval Map.empty exampleExp
  print $ runHead $ eval Map.empty exampleExp
  print $ runInList $ eval Map.empty exampleExp
  --
  print $ runTransformerStack $ eval Map.empty (Plus (Lit 1) (Lambda "x" (Var "x")))
  print $ (runTransformerStack $ eval Map.empty exampleExpBad)

