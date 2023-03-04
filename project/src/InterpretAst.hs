module InterpretAst ( interpretAST ) where
import DataStructures
import Lib

-- La fonction `expr` analyse une expression SExpr et la retourne sous forme d'une autre structure d'expression.
expr :: SExpr -> SExpr
expr NullExpr = NullExpr -- Si l'expression est NullExpr, elle est retournée telle quelle.
expr (NumExpr n) = NumExpr n -- Si l'expression est un NumExpr, elle est retournée telle quelle.
expr (BoolExpr b) = BoolExpr b -- Si l'expression est un BoolExpr, elle est retournée telle quelle.
expr (SymbolExpr v) = SymbolExpr v -- Si l'expression est un SymbolExpr, elle est retournée telle quelle.
expr pair@(Pair _ _) -- Si l'expression est une paire (Pair), elle est analysée pour déterminer son type.
  | isIf pair = ifExpr pair -- Si l'expression est une expression `if`, la fonction `ifExpr` est appelée pour la traiter.
  | isLambda pair = lambdaExpr pair -- Si l'expression est une expression `lambda`, la fonction `lambdaExpr` est appelée pour la traiter.
  | isDefine pair = defineExpr pair -- Si l'expression est une expression `define`, la fonction `defineExpr` est appelée pour la traiter.
  | otherwise = appExpr pair -- Sinon, c'est une expression d'application, et la fonction `appExpr` est appelée pour la traiter.

-- La fonction `isIf` détermine si une expression est une expression `if`.
isIf :: SExpr -> Bool
-- Si la première expression dans la paire est un SymbolExpr avec la valeur "if", alors c'est une expression `if`.
isIf (Pair (SymbolExpr v) _) = v == "if"
-- Sinon, ce n'est pas une expression `if`.
isIf _ = False

-- La fonction `ifExpr` construit une structure d'expression pour une expression `if`.
ifExpr :: SExpr -> SExpr
-- Elle analyse les expressions pour le prédicat, le clause `then`, et le clause `else` et les retourne dans une structure `IfExpr`.
ifExpr (Pair _ (Pair p (Pair t (Pair e NullExpr)))) =
  IfExpr (expr p) (expr t) (expr e)

-- La fonction `isLambda` détermine si une expression est une expression `lambda`.
isLambda :: SExpr -> Bool
-- Si la première expression dans la paire est un SymbolExpr avec la valeur "lambda", alors c'est une expression `lambda`.
isLambda (Pair (SymbolExpr v) _) = v == "lambda"
-- Sinon, ce n'est pas une expression `lambda`.
isLambda _ = False

-- La fonction `lambdaExpr` prend une `SExpr` et la convertit en une `LambdaExpr`.
lambdaExpr :: SExpr -> SExpr
lambdaExpr (Pair _ (Pair params (Pair bdy NullExpr))) =
  LambdaExpr (toSExprList params) (expr bdy)

-- La fonction `isDefine` vérifie si une `SExpr` est une définition.
isDefine :: SExpr -> Bool
isDefine (Pair (SymbolExpr v) _) = v == "define"
isDefine _ = False

-- La fonction `defineExpr` prend une `SExpr` et la convertit en une expression de définition.
defineExpr :: SExpr -> SExpr
defineExpr (Pair _ (Pair v (Pair val NullExpr))) =
  DefExpr v (expr val)

-- La fonction `appExpr` prend une `SExpr` et la convertit en une expression d'application.
appExpr :: SExpr -> SExpr
appExpr (Pair op args) =
  AppExpr (expr op) (map expr (toSExprList args))

-- La fonction `interpretAST` interprète une liste d'expressions `SExpr` en une liste d'expressions interprétées.
interpretAST :: [SExpr] -> [SExpr]
interpretAST = map expr
