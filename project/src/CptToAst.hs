module CptToAst (cptToAst, Ast(..)) where
import Data.Maybe
import Data.List()

data Cpt = VarInt Int
          | VarString String
          | VarList [Cpt]
          | VarChar Char
          | VarBool Bool
          | VarFloat Float
          deriving (Eq, Ord, Show)

data Ast = LiteralInt Int
         | LiteralString String
         | LiteralFloat Float
         | LiteralBool Bool
         | LiteralChar Char
         | FunctionCall String [Ast]
         | FunctionDecl String [Ast] Ast
         | VariableDecl String Ast
         | VariableRef String
         | While Ast Ast
         | If Ast Ast (Maybe Ast)
         | BinaryOp String Ast Ast
         | ReturnAst Ast
         | Block [Ast]
         deriving (Eq, Ord, Show)

getCpt :: Maybe (Cpt, [Char]) -> Cpt
getCpt (Just (cpt, _)) = cpt
getCpt Nothing = error "Error: Cannot get Cpt from Nothing"

cptToAst :: Cpt -> Maybe Ast
cptToAst (VarInt i) = Just $ LiteralInt i
cptToAst (VarFloat f) = Just $ LiteralFloat f
cptToAst (VarString s) = Just $ LiteralString s
cptToAst (VarChar c) = Just $ LiteralChar c
cptToAst (VarBool b) = Just $ LiteralBool b
cptToAst (VarList [VarString "function", VarString _, VarString functionName, VarList args, body]) = -- `VarString _` == returnType (return de fonction)
    Just $ FunctionDecl functionName (mapMaybe cptToAst args) (fromMaybe (Block []) (cptToAst body))
cptToAst (VarList [VarString "var", VarString _, VarString varName, VarString "=", value]) = -- `VarString _` == varType (type de la variable)
    Just $ VariableDecl varName (fromMaybe (LiteralString "") (cptToAst value))
cptToAst (VarList [VarString "=", left, VarString op, right]) =
    Just $ BinaryOp op (fromMaybe (LiteralString "") (cptToAst left)) (fromMaybe (LiteralString "") (cptToAst right))
cptToAst (VarList [VarString "if", condition, trueBranch]) =
    Just $ If (fromMaybe (LiteralString "") (cptToAst condition)) (fromMaybe (Block []) (cptToAst trueBranch)) Nothing
cptToAst (VarList [VarString "if", condition, trueBranch, VarString "else", falseBranch]) =
    Just $ If (fromMaybe (LiteralString "") (cptToAst condition)) (fromMaybe (Block []) (cptToAst trueBranch)) (Just (fromMaybe (Block []) (cptToAst falseBranch)))
cptToAst (VarList [VarString "while", condition, body]) =
    Just $ While (fromMaybe (LiteralString "") (cptToAst condition)) (fromMaybe (Block []) (cptToAst body))
cptToAst (VarList [VarString "return", value]) =
    Just $ ReturnAst (fromMaybe (LiteralString "") (cptToAst value))
cptToAst (VarList statements) = Just $ Block (mapMaybe cptToAst statements)
cptToAst (VarList [left, VarString op, right])
    | op `elem` ["==", "!=", ">", "<", ">=", "<=", "+", "-", "*", "/", "%", "+=", "-=", "*=", "/=", "%=", "||", "&&"] =
        Just $ BinaryOp op (fromMaybe (LiteralString "") (cptToAst left)) (fromMaybe (LiteralString "") (cptToAst right))
cptToAst _ = Nothing
