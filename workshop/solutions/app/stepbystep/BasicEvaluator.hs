
-- module BasicEvaluator where

import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

-- | Naive non-monadic evaluator - ignores errors, uses environment as a parameter.
eval :: Env -> Exp -> Value

eval env (Lit i) = IntVal i

eval env (Var var) =
  fromJust (Map.lookup var env)  -- fromJust :: Maybe a -> a.
  -- Fails if the variable does not exist in the environment and lookup returns Nothing.

eval env (Plus expr1 expr2) =
  let
    -- The pattern match of the evaluated result against IntVal is allowed.
    -- But if the result happens to be a FunVal and not an IntVal there will be a runtime failure.
    IntVal val1 = eval env expr1
    IntVal val2 = eval env expr2
  in IntVal (val1 + val2 )

-- Note that the expression of a function may include any number of variables.
-- One of these may be the lambda variable.
-- Others may be variables appearing the environment: variables over which the function is closed.
-- Still others may be free variables to be abstracted or closed over later.
eval env (Lambda var expr) = FunVal env var expr

eval env (Apply expr1 expr2 ) =
  let
    fun = eval env expr1
    val = eval env expr2
  in case fun of
    FunVal closureEnv var body -> eval (Map.insert var val closureEnv) body
    -- Fails if expr1 does not return a function.

exampleExp = Lit 12 `Plus` (Apply (Lambda "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (Apply (Lambda "x" (Var "y")) (Lit 4 `Plus` Lit 2))

main :: IO ()

main = do
  print $ eval Map.empty exampleExp
  -- print $ eval Map.empty exampleExpBad
  -- print $ eval Map.empty (Apply (Lit 5) (Lit 4 `Plus` Lit 2))


