
-- module MonadicEvaluator where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe (Maybe, fromJust)
import qualified Data.Map as Map (lookup, empty, insert)
import EvaluatorTypes (VarName, Exp(Lit, Var, Plus, Lambda, Apply), Value(IntVal, FunVal), Env)
import EvaluatorTypes (returnValueOrFail)

{-
  Evaluate expressions inside a generic monad.

  Delay the choice of the actual monad to use to the call site by using a runner
  of the right type to extract the embedded computed value. For example, to force the
  identity monad to be used, extract the computed value by using runIdentity.
-}

-- | Monadic evaluator - generic in the monad's data type.
eval :: Monad monad => Env -> Exp -> monad Value

eval env (Lit i) = return $ IntVal i

eval env (Var var) =
  let
      maybeValue = Map.lookup var env
      message = "undefined variable: " ++ var
  in returnValueOrFail maybeValue message

eval env (Plus expr1 expr2) = do
  IntVal i1 <- eval env expr1
  IntVal i2 <- eval env expr2
  return $ IntVal (i1 + i2)

eval env (Lambda var expr) = return $ FunVal env var expr

eval env (Apply expr1 expr2) = do
  fun <- eval env expr1
  val <- eval env expr2
  case fun of
    FunVal closureEnv name body ->
      eval (Map.insert name val closureEnv) body

exampleExp = Lit 12 `Plus` (Apply (Lambda "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (Apply (Lambda "x" (Var "y")) (Lit 4 `Plus` Lit 2))

maybeId :: Maybe a -> Maybe a
maybeId = id

listId :: [a] -> [a]
listId = id

main :: IO ()

main = do
  print $ runIdentity $ eval Map.empty exampleExp -- evaluate in Identity
  print $ fromJust $ eval Map.empty exampleExp -- evaluate in Maybe
  print $ maybeId $ eval Map.empty exampleExp -- evaluate in Maybe
  print $ (!! 0) $ eval Map.empty exampleExp -- evaluate in list
  print $ listId $ eval Map.empty exampleExp -- evaluate in list
  result <- eval Map.empty exampleExp -- evaluate in IO monad
  print result


