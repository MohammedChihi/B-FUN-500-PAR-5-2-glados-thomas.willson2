module Ast (Ast(..), evalAST) where

import Cpt
import Data.Maybe
-- import Data.Bool
-- import Data.List
import qualified Data.Map as Map

data Ast = Define String Ast
         | AstInt Int
         | AstSymbol String
         | AstBool Bool
         | AstList [Ast]
         | Call String [Ast]
         deriving Show

type Env = Map.Map String Ast

-- for HUnit (assertEqual)
instance Eq Ast where
    (AstInt a) == (AstInt b) = a == b
    (AstBool a) == (AstBool b) = a == b
    (AstSymbol a) == (AstSymbol b) = a == b
    (Define a1 a2) == (Define b1 b2) = a1 == b1 && a2 == b2
    _ == _ = False

-- cptToAST :: Cpt -> Maybe Ast
-- cptToAST cpt = case cpt of
--   (VarInt i) -> Just (AstInt i)
--   (VarString s) -> Just (AstSymbol s)
--   (VarList lst) -> case lst of
--     (VarString "define":VarString var:val:xs) -> do
--       valAst <- cptToAST val
--       if (null xs)
--         then Just (Define var valAst)
--         else Nothing
--     (VarString op:xs) -> if elem op ["+","-","*","div","mod","eq?","<"]
--         then Just (Call op (mapMaybe cptToAST xs))
--         else Nothing
--     _ -> Nothing

-- Test 1
-- cpt = VarList [VarString "+", VarString "x", VarInt 4]
-- cptToAST cpt
-- Output: Just (Call "+" [AstSymbol "x", AstInt 4])

-- Test 2
-- cpt = VarList [VarString "+", VarString "x", VarList [VarString "*", VarInt 4, VarString "y"]]
-- cptToAST cpt
-- Output: Just (Call "+" [AstSymbol "x", Call "*" [AstInt 4, AstSymbol "y"]])

-- Test 3
-- cpt = VarList [VarString "define", VarString "fourtyTwo", VarList [VarString "*", VarInt 7, VarInt 6]]
-- cptToAST cpt
-- Output: Just (Define "fourtyTwo" (Call "*" [AstInt 7, AstInt 6]))

evalOp :: (Int -> Int -> Int) -> Env -> [Ast] -> Maybe Ast
evalOp op env args = case mapMaybe (evalAST env) args of
    (AstInt a:AstInt b:_) -> Just (AstInt (a `op` b))
    _ -> Nothing

evalDiv :: Env -> [Ast] -> Maybe Ast
evalDiv env args = case mapMaybe (evalAST env) args of
    (AstInt a:AstInt b:_) -> if b == 0 then Nothing else Just (AstInt (a `div` b))
    _ -> Nothing

evalMod :: Env -> [Ast] -> Maybe Ast
evalMod env args = case mapMaybe (evalAST env) args of
    (AstInt a:AstInt b:_) -> if b == 0 then Nothing else Just (AstInt (a `mod` b))
    _ -> Nothing

evalEq :: Env -> [Ast] -> Maybe Ast
evalEq env args = case mapMaybe (evalAST env) args of
    (AstInt a:AstInt b:_) -> if a == b then Just (AstBool True) else Just (AstBool False)
    _ -> Nothing

evalLess :: Env -> [Ast] -> Maybe Ast
evalLess env args = case mapMaybe (evalAST env) args of
    (AstInt a:AstInt b:_) -> if a < b then Just (AstBool True) else Just (AstBool False)
    _ -> Nothing

evalIf :: Env -> [Ast] -> Maybe Ast
evalIf env args = case args of
    [AstBool cond, thenExpr, elseExpr] -> if cond then evalAST env thenExpr else evalAST env elseExpr
    [condExpr, thenExpr, elseExpr] -> case evalAST env condExpr of
        Just (AstBool cond) -> if cond then evalAST env thenExpr else evalAST env elseExpr
        _ -> Nothing
    _ -> Nothing

functionPointer :: [(String, Env -> [Ast] -> Maybe Ast)]
functionPointer = [("+", evalOp (+)), ("-", evalOp (-)), ("*", evalOp (*)), ("div", evalDiv),
                   ("mod", evalMod), ("eq?", evalEq), ("<", evalLess), ("if", evalIf)]

evalAST :: Env -> Ast -> Maybe Ast
evalAST env ast = case ast of
    AstInt i -> Just (AstInt i)
    AstBool b -> Just (AstBool b)
    AstSymbol s -> Map.lookup s env
    Call op args ->
        let evalFunc = lookup op functionPointer
        in case evalFunc of
            Just f -> f env args
            Nothing -> Nothing
    _ -> Nothing

-- intoEnv :: Ast -> Env -> Env
-- intoEnv (Define symbol ast) env = Map.insert symbol ast env
