module AstTests (
) where

-- import Ast
-- import BuiltIn
-- import Test.HUnit

-- testEvalInt :: Test
-- testEvalInt = TestCase (assertEqual "Eval int" (Just (AstInt 42)) (evalAST env (AstInt 42)))

-- testEvalBool :: Test
-- testEvalBool = TestCase (assertEqual "Eval bool" (Just (AstBool True)) (evalAST (AstBool True)))

-- testEval_Add_Mul_Div :: Test
-- testEval_Add_Mul_Div = TestCase (assertEqual "Eval add mul div" (Just (AstInt 11)) (evalAST (Call "+" [Call "*" [AstInt 2, AstInt 3], Call "div" [AstInt 10, AstInt 2]])))

-- testEval_Eq_Mul_Sub :: Test
-- testEval_Eq_Mul_Sub = TestCase (assertEqual "Eval eq? mul sub" (Just (AstBool True)) (evalAST (Call "eq?" [Call "*" [AstInt 2, AstInt 5], Call "-" [AstInt 11, AstInt 1]])))

-- testEval_Less_Mod :: Test
-- testEval_Less_Mod = TestCase (assertEqual "Eval less mod" (Just (AstBool False)) (evalAST (Call "<" [AstInt 1, Call "mod" [AstInt 10, AstInt 3]])))

-- testEvalIf1 :: Test
-- testEvalIf1 = TestCase (assertEqual "Eval if basic (1)" (Just (AstInt 1)) (evalAST (Call "if" [AstBool True, AstInt 1, AstInt 2])))

-- testEvalIf2 :: Test
-- testEvalIf2 = TestCase (assertEqual "Eval if basic (2)" (Just (AstInt 2)) (evalAST (Call "if" [AstBool False, AstInt 1, AstInt 2])))

-- testEvalIf3 :: Test
-- testEvalIf3 = TestCase (assertEqual "Eval if advanced" (Just (AstInt 42)) (evalAST (Call "if" [Call "eq?" [AstInt 5, AstInt 4], Call "+" [AstInt 13, AstInt 4], Call "*" [AstInt 21, AstInt 2]])))

-- testAst :: Test
-- testAst = TestList [
--     TestLabel "test Eval Int" testEvalInt, TestLabel "test Eval Bool" testEvalBool, TestLabel "Test Eval Add Mul Div" testEval_Add_Mul_Div
--     , TestLabel "test Eval Eq Mul Sub" testEval_Eq_Mul_Sub, TestLabel "test Eval Less Mod" testEval_Less_Mod
--     , TestLabel "test Eval if 01" testEvalIf1, TestLabel "test Eval if 02" testEvalIf2, TestLabel "test Eval if 02" testEvalIf3
--     ]
