module BuiltIn ( initBuiltIn, insertBuiltIn ) where
import DataStructures
import qualified Data.Map as Map
import System.IO()

-- | Evaluate an arithmetic expression with the specified binary operator.
evalAritmethic :: (Int -> Int -> Int) -- ^ The binary operator to use
               -> [SExpr] -- ^ The list of arguments (two numbers)
               -> SExpr -- ^ Resulting expression (a number)
-- If both arguments are numbers, apply the operator and return the result.
evalAritmethic op [(NumExpr x), (NumExpr y)] = NumExpr (x `op` y)
-- If the arguments are not both numbers, return an exception.
evalAritmethic _ _ = Exception "on ore two parameters are false."

-- | Evaluate a division expression.
evalDiv :: [SExpr] -- ^ The list of arguments (two numbers)
        -> SExpr -- ^ Resulting expression (a number)
-- If both arguments are numbers, perform the division.
evalDiv [(NumExpr x), (NumExpr y)] = if y == 0 then Exception "division by zero." else NumExpr (x `div` y)
-- If the arguments are not both numbers, return an exception.
evalDiv _ = Exception "on ore two parameters are false."

-- | Evaluate a modulo expression.
evalMod :: [SExpr] -- ^ The list of arguments (two numbers)
        -> SExpr -- ^ Resulting expression (a number)
-- If both arguments are numbers, perform the modulo.
evalMod [(NumExpr x), (NumExpr y)] = if y == 0 then Exception "modulo by zero." else NumExpr (x `mod` y)
-- If the arguments are not both numbers, return an exception.
evalMod _ = Exception "on ore two parameters are false."

-- | Evaluate a comparison expression.
evalPredicates :: Int -- ^ The operator to use (0 for equality, 1 for less than)
               -> [SExpr] -- ^ The list of arguments (two numbers)
               -> SExpr -- ^ Resulting expression (a boolean)
-- If both arguments are numbers, perform the comparison.
evalPredicates op [(NumExpr x), (NumExpr y)] = if op == 0 then BoolExpr (x == y) else BoolExpr (x < y)
-- If the arguments are not both numbers, return a false boolean.
evalPredicates _ _ = BoolExpr False

-- | Define the built-in functions for our language.
functionPointer :: [(String, SExpr)]
functionPointer = [ ("+", Procedure (evalAritmethic (+))),  ("sub", Procedure (evalAritmethic (-))),  ("*", Procedure (evalAritmethic (*))),  ("div", Procedure (evalDiv)), ("mod", Procedure (evalMod)), ("eq?", Procedure (evalPredicates 0)),  ("<", Procedure (evalPredicates 1))]

-- | Create the built-in environment.
insertBuiltIn :: Env
insertBuiltIn = Env (Map.fromList functionPointer) EmptyEnv

-- | Initialize the built-in environment.
initBuiltIn :: IO Env
initBuiltIn = return insertBuiltIn
