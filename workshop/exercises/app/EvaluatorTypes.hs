
module EvaluatorTypes
  (
    VarName,
    Exp(..),
    Value(..),
    Env,
    returnValueOrFail
  ) where

import qualified Data.Map as Map (Map)

{-
  Basic data types for expression evaluator.
-}

-- | Variable names.
type VarName = String -- variable names

-- | Expressions.
data Exp =
    Lit Integer
  | Var VarName
  | Plus Exp Exp
  | Lambda VarName Exp -- lambda abstraction
  | Apply Exp Exp -- function application
    deriving (Show)

-- | Values.
-- A function value has a single lambda variable, and an expression
-- in terms of that variable and other variables, over some of which
-- the function is closed. The values of the other variables over which
-- the function is closed are provided in the environment field
-- of a function value.
data Value =
    IntVal Integer
  | FunVal Env VarName Exp
    deriving (Show)

-- | Evaluation environment.
type Env = Map.Map VarName Value

{-
  Reminder - Types of functions used in failure processing.

    maybe :: b -> (a -> b) -> Maybe a -> b
        applies the function to the Just value or uses a provided default value for the function
    fail :: String -> m a
        fails with a message - has the right monadic type
-}

{-
   Embed a Just value of a Maybe in another monad
   or fail in that other monad if the Maybe is Nothing.
-}
returnValueOrFail :: Monad m => Maybe a -> String -> m a
returnValueOrFail maybeValue message = maybe (fail message) return $ maybeValue