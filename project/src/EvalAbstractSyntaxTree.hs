module EvalAbstractSyntaxTree ( evalAST ) where
import DataStructures
import qualified Data.Map as Map

-- evaluate est la fonction principale qui évalue une expression
evaluate :: Env -> SExpr -> (Env, SExpr)
evaluate env expr =
  -- on utilise le pattern matching pour déterminer le type de l'expression
  case expr of
    -- si c'est NullExpr, on utilise la fonction handleNull
    NullExpr -> handleNull env
    -- si c'est BoolExpr, on utilise la fonction handleBoolean
    BoolExpr bool -> handleBoolean env bool
    -- si c'est NumExpr, on utilise la fonction handleNumber
    NumExpr num -> handleNumber env num
    -- si c'est SymbolExpr, on utilise la fonction handleSymbol
    SymbolExpr var -> handleSymbol env var
    -- si c'est LambdaExpr, on utilise la fonction handleLambda
    LambdaExpr params body -> handleLambda env params body
    -- si c'est DefExpr, on utilise la fonction handleDefinition
    DefExpr var nexpr -> handleDefinition env var nexpr
    -- si c'est IfExpr, on utilise la fonction handleIf
    IfExpr cond thenClause elseClause -> handleIf env cond thenClause elseClause
    -- si c'est AppExpr, on utilise la fonction handleApplication
    AppExpr op args -> handleApplication env op args
    -- dans tous les autres cas, on utilise handleError
    _ -> handleError env

-- handleNull renvoie simplement l'environnement et NullExpr
handleNull :: Env -> (Env, SExpr)
handleNull env = (env, NullExpr)

-- handleBoolean renvoie simplement l'environnement et BoolExpr
handleBoolean :: Env -> Bool -> (Env, SExpr)
handleBoolean env bool = (env, BoolExpr bool)

-- handleNumber renvoie simplement l'environnement et NumExpr
handleNumber :: Env -> Int -> (Env, SExpr)
handleNumber env num = (env, NumExpr num)

-- getValue renvoie la valeur d'une variable donnée dans l'environnement
getValue :: Env -> String -> SExpr
-- si la variable n'est pas trouvée dans l'environnement, renvoie une exception
getValue EmptyEnv var = Exception ("variable " ++ var ++ " is not bound.")
getValue env@(Env frame parent) var =
  -- on recherche d'abord la valeur de la variable dans le cadre courant
  case Map.lookup var frame of
    Just result -> result
    -- sinon, on cherche dans le cadre parent
    Nothing     -> getValue parent var

-- handleSymbol renvoie l'environnement et la valeur de la variable
handleSymbol :: Env -> String -> (Env, SExpr)
handleSymbol env var = (env, getValue env var)

-- handleLambda gère l'expression de lambda en retournant un environnement
-- mis à jour avec la valeur de la fonction lambda et la forme lambda elle-même
handleLambda :: Env -> [SExpr] -> SExpr -> (Env, SExpr)
handleLambda env params body = (env, LambdaExpr params body)

-- insertBinding insère une nouvelle valeur de liaison dans l'environnement actuel
insertBinding :: Env -> String -> SExpr -> Env
insertBinding (Env frame parent) name value = Env (Map.insert name value frame) parent

-- handleDefinition gère la définition en évaluant la valeur d'expression et en insérant
-- le nouveau symbole dans l'environnement actuel
handleDefinition :: Env -> SExpr -> SExpr -> (Env, SExpr)
handleDefinition env var expr =
  ((insertBinding env symbol value), value)
  where (_, value) = evaluate env expr
        (SymbolExpr symbol) = var

-- handleIf gère la condition if en évaluant la valeur d'expression de prédicat et en décidant
-- si la clause then ou else doit être évaluée
handleIf :: Env -> SExpr -> SExpr -> SExpr -> (Env, SExpr)
handleIf env cond thenClause elseClause =
  if result == True then evaluate newEnv thenClause
  else evaluate newEnv elseClause
  where (newEnv, (BoolExpr result)) = evaluate env cond

-- augmentEnv augmente l'environnement en créant un nouveau cadre de liaison
-- avec les nouveaux paramètres et arguments
augmentEnv :: Env -> [SExpr] -> [SExpr] -> Env
augmentEnv parent params args = Env (Map.fromList (zip (map renderSExpr params) args)) parent

-- evaluateList évalue chaque expression de la liste en utilisant l'environnement
-- actuel et retourne la liste des résultats évalués et l'environnement final
evaluateList :: Env -> [SExpr] -> (Env, [SExpr])
evaluateList env [] = (env, [])
evaluateList env (expr:exprs) =
  let (updatedEnv, resultFirst) = evaluate env expr
      (finalEnv, results) = evaluateList updatedEnv exprs
  in (finalEnv, resultFirst : results)

-- handleApplication gère l'application de la fonction en évaluant le premier opérateur
-- en utilisant l'environnement actuel, puis en évaluant les arguments et en déterminant
-- si l'opérateur est une procédure, une forme lambda ou une erreur
handleApplication :: Env -> SExpr -> [SExpr] -> (Env, SExpr)
handleApplication env op args =
  let (firstEnv, operator) = evaluate env op
      (finalEnv, arguments) = evaluateList firstEnv args
  in case operator of
       Procedure proc -> (finalEnv, proc arguments)
       LambdaExpr params body -> (finalEnv, result)
         where env' = augmentEnv finalEnv params arguments
               (_, result) = evaluate env' body
       _ -> (firstEnv, Exception "attempt to apply non-procedure.")

-- handleError prend un environnement en entrée et retourne toujours un couple d'environnement et d'expression avec une exception "undefined".
handleError :: Env -> (Env, SExpr)
handleError env = (env, Exception "undefined.")

-- evalAST prend un environnement et une liste d'expressions en entrée.
-- Si la liste est vide, retourne un couple d'environnement et d'expression nulle.
-- Si la liste n'a qu'un élément, évalue l'expression unique dans l'environnement donné et retourne le résultat.
-- Sinon, évalue la première expression dans l'environnement donné, ignore le résultat et passe à l'évaluation des expressions restantes dans l'environnement mis à jour.
evalAST :: Env -> [SExpr] -> (Env, SExpr)
evalAST env [] = (env, NullExpr)
evalAST env [expr] = evaluate env expr
evalAST env (expr:exprs) =
  let (envAfterFirstExpr, _) = evaluate env expr
  in evalAST envAfterFirstExpr exprs
