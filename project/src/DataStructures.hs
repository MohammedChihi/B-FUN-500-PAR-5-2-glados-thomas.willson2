module DataStructures ( Data, Env(..), SExpr(..), renderSExpr ) where
import qualified Data.Map as Map

-- Type `Data` représente une map entre une chaîne de caractères (`String`) et une expression s-expr (`SExpr`)
type Data = Map.Map String SExpr

-- Env est un type qui représente l'environnement pour les expressions s-expr. 
-- Il peut être vide (`EmptyEnv`) ou contenir une map de données (`Data`) et un environnement parent (`Env`)
data Env = EmptyEnv | Env Data Env

-- data Env = [(String, SExpr)]

-- SExpr est un type qui représente les expressions s-expr dans notre système
-- Il peut être un nombre entier (`NumExpr`), une expression booléenne (`BoolExpr`), une expression symbolique (`SymbolExpr`),
-- une paire d'expressions s-expr (`Pair`), une exception (`Exception`), une expression lambda (`LambdaExpr`),
-- une procédure (`Procedure`), une application de procédure (`AppExpr`), une expression de définition (`DefExpr`) ou une expression conditionnelle (`IfExpr`)
data SExpr = NullExpr | NumExpr Int | BoolExpr Bool | SymbolExpr String | Pair SExpr SExpr | Exception String | LambdaExpr [SExpr] SExpr | Procedure ([SExpr] -> SExpr) | AppExpr SExpr [SExpr] | DefExpr SExpr SExpr | IfExpr SExpr SExpr SExpr

-- L'instance `Show` pour SExpr permet d'afficher une expression s-expr en utilisant la fonction `show`
instance Show SExpr where
  show = renderSExpr

-- `renderSExpr` prend une expression s-expr et la retourne sous forme de chaîne de caractères
renderSExpr :: SExpr -> String
-- Pour un `NumExpr`, on retourne sa valeur numérique en tant que chaîne de caractères
renderSExpr (NumExpr num) = show num
-- Pour un `BoolExpr`, on retourne "#t" si la valeur est vraie, sinon "#f"
renderSExpr (BoolExpr bool) = if bool then "#t" else "#f"
-- Pour un `SymbolExpr`, on retourne simplement sa valeur de chaîne de caractères
renderSExpr (SymbolExpr var) = var
-- Pour une `Exception`, on retourne le message d'erreur précédé de "*** ERROR : "
renderSExpr (Exception msg) = "*** ERROR : " ++ msg
-- Pour un `Pair`, on retourne sa première expression s-expr entre parenthèses, suivie de la seconde expression séparées par une flèche " . ".
renderSExpr (Pair first second) = "(" ++ renderSExpr first ++ " . " ++ renderSExpr second ++ ")"
-- Pour une LambdaExpr, on retourne toujours "#<procedure>"
renderSExpr (LambdaExpr _ _) = "#<procedure>"
-- Pour une Procedure, on retourne toujours "FP", car lorsque nous allons prendre le résultat et le vérifier dans la fonction qui gère le tout
renderSExpr (Procedure _) = "FP"
-- Pour tous les autres cas, on retourne une chaîne vide ""
renderSExpr _ = ""
