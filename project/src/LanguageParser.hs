module LanguageParser(
    module LanguageParser
) where

import ParserBootstrap
import Control.Applicative
import Cpt

-- Check if the given character is a separator
checkSep :: Char -> Bool
checkSep '(' = True
checkSep ')' = True
checkSep '[' = True
checkSep ']' = True
checkSep '"' = True
checkSep '\'' = True
checkSep ' ' = True
checkSep ';' = True
checkSep ',' = True
checkSep _ = False

-- Check if the given character is an Empty Spaces
checkEmptySpaces :: Char -> Bool
checkEmptySpaces '\n' = True
checkEmptySpaces '\t' = True
checkEmptySpaces ' ' = True
checkEmptySpaces _ = False

-- Get the first word of a given string
getWord :: String -> String
getWord [] = ""
getWord (x:xs) = if checkSep x == True then "" else x : getWord xs

-- Skip the first word and Get the rest of the string
getRest :: String -> String
getRest [] = ""
getRest (x:xs) = if checkSep x == True then x:xs else getRest xs

-- Return a maybe(VarString "str", rest) else Nothing
-- send a str to find in another string, if find
parseWord :: String -> Parser Cpt
parseWord cmpr = Parser {
    runParser = \str -> case ((getWord str) == cmpr) of 
        True -> Just(VarString cmpr, getRest str)
        False -> Nothing
}

-- Return a maybe(VarString "str", rest) else Nothing
-- try to parse a string as the first word
parseMyString :: Parser Cpt
parseMyString = Parser {
    runParser = \str -> case getWord str of
            "" -> Nothing
            word -> Just(VarString word, getRest str)
}

-- Return a maybe(VarFloat nb, rest) else Nothing
-- try to parse a float as the first word
parseFloat :: Parser Cpt
parseFloat = Parser {
    runParser = \str -> case getWord str of
        "" -> Nothing
        exprt -> case (runParser ((\s -> read s :: Float) <$> (parseSome $ parseAnyChar  (['0'..'9'] ++ ['.']))) exprt) of
            Nothing-> Nothing
            Just(res, _) -> Just(VarFloat res, getRest str)
}

-- Return a maybe(VarString, "") else Nothing
-- try to parse the first characters, if they are Empty spaces or newlines, delete them
parseEmptySpaces :: Parser String
parseEmptySpaces = parseSome (parseAnyChar ([' ', '\n', '\t']))

parseCptInt :: Parser Cpt
parseCptInt = VarInt <$> parseInt

parseBoolean :: Parser Cpt
parseBoolean = Parser $ \str -> case runParser parseMyString str of
    Nothing -> Nothing
    Just(VarString res, rest) | res == "True" -> Just(VarBool True, rest)
                              | res == "False" -> Just(VarBool False, rest)
                              | otherwise -> Nothing
    _ -> Nothing

parseOperator :: Parser Cpt
parseOperator = Parser $ \str -> case runParser (parseSome (parseAnyChar ['=', '+', '-', '*', '/', '%', '<', '>', '!', '|', '&', '^', '~'])) str of
    Nothing -> Nothing
    Just("", _) -> Nothing
    Just(res, rest) -> Just(VarString res, rest)

parseArg :: Parser Cpt
parseArg = parseEmptySpaces *> (
    parseCptInt
    <|> parseFloat
    <|> parseBoolean
    <|> parseStringDeclaration
    <|> parseCharDeclaration
    <|> parseMyString
    <|> parseOperations
    <|> parseFunctionCall
    )

parseArg2 :: Parser Cpt
parseArg2 = parseCptInt
    <|> parseFloat
    <|> parseBoolean
    <|> parseStringDeclaration
    <|> parseCharDeclaration
    <|> parseOperations
    <|> parseFunctionCall2
    <|> parseMyString

parseToDelim :: Parser Cpt
parseToDelim = Parser $ \inpt -> case runParser (parseSome $ parseAnyChar (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['+','-','*','/','%', '?', '#', '/', '@', '$', '^', '&', '_', '.', '~', '|', '\\', '`', ':', ' ', '\n', '\t', '(', ')', '[', ']', '>', '<'])) inpt of
    Nothing -> Nothing
    Just(res, rest) -> Just(VarString res, rest) 

parseFunctionCall2 :: Parser Cpt
parseFunctionCall2 = VarList <$> ((++) <$> (\a -> [a]) <$>  (parseChar '(' *> parseMyString) <*> ((\a -> [a]) <$> (parseParameter)))

parseParenthesisReturn :: Parser Cpt
parseParenthesisReturn = VarList <$> ((++) <$> (\a -> [a]) <$> (parseWord "return" <* parseEmptySpaces <* parseChar '(') <*> ((\a -> [a]) <$> (parseArg2) <* parseChar ')' <* parseChar ';'))

parseReturn :: Parser Cpt
parseReturn = VarList <$> ((++) <$> (\str -> [str]) <$> (parseWord "return") <*> ((\a -> [a]) <$>  (parseArg <* parseChar ';')))

parseStringDeclaration :: Parser Cpt
parseStringDeclaration = Parser $ \input -> case runParser (parseChar '"' *> parseToDelim <* parseChar '"') input of
    Nothing -> Nothing
    Just(VarString res, rest) -> Just(VarString res, rest)
    _ -> Nothing

parseCharDeclaration :: Parser Cpt
parseCharDeclaration = Parser $ \input -> case runParser (parseChar '\'' *> parseMyString <* parseChar '\'') input of
    Nothing -> Nothing
    Just(VarString (res:_), rest) -> Just(VarChar res, rest)
    _ -> Nothing

parseListAssignation :: Parser Cpt
parseListAssignation = VarList <$> ((++) <$> (\a -> [a]) <$> (parseChar '[' *> parseVarType) <*> ((parseMany (parseChar ',' *> parseEmptySpaces *> parseVarType)) <* parseChar ']'))

parseListDeclaration :: Parser Cpt
parseListDeclaration = Parser $ \input -> case runParser parseMyString input of
    Nothing -> Nothing
    Just(VarString str, xs) -> case runParser (parseChar '[') xs of
        Nothing -> Nothing
        Just(c1, ys) -> case runParser (parseChar ']') ys of
            Nothing -> Nothing
            Just(c2, rest) -> Just(VarString (str++[c1]++[c2]), rest)
    _ -> Nothing

parseVarType :: Parser Cpt
parseVarType = parseCharDeclaration
        <|> parseStringDeclaration
        <|> parseCptInt
        <|> parseFloat
        <|> parseBoolean
        <|> parseFunctionCall
        <|> parseMyString

parseDeclarationTypes :: Parser Cpt
parseDeclarationTypes = parseListDeclaration
    <|> parseWord "int"
    <|> parseWord "char"
    <|> parseWord "string"
    <|> parseWord "float"
    <|> parseWord "int"
    <|> parseWord "boolean"

parseAssignation :: Parser Cpt
parseAssignation = VarList <$> ((++) <$> (\a -> [a]) <$> VarChar <$> (parseChar '=' <* parseEmptySpaces) <*> ((\a -> [a]) <$> (((parseOperations <|> parseListAssignation) <* parseChar ';') <|> parseFunctionCall)))

parseDeclaration :: Parser Cpt
parseDeclaration = VarList <$> ((++) <$> (\str -> [str]) <$> (parseWord "var" <* parseEmptySpaces) <*> ((++) <$> ((\str -> [str]) <$> (parseDeclarationTypes <* parseEmptySpaces)) <*> ((++) <$>  (\a -> [a]) <$> (parseMyString <* parseEmptySpaces) <*> ((\a -> [a]) <$> parseAssignation))))

extractOperations :: Parser Cpt
extractOperations = ((parseEmptySpaces *> parseOperator) <|> parseOperator <|> (parseEmptySpaces *> parseVarType) <|> parseVarType)

parseOperations :: Parser Cpt
parseOperations = VarList <$> ((++) <$> (\a -> [a]) <$> (parseVarType) <*> (parseMany (extractOperations)))

parseParenthesisExpression :: Parser Cpt
parseParenthesisExpression = VarList <$> ((\a -> [a]) <$> (parseChar '(' *> parseOperations <* parseChar ')'))

parseExpressionManipulation :: Parser Cpt
parseExpressionManipulation = parseParenthesisExpression
    <|> parseOperations

cptStringToString :: Cpt -> String
cptStringToString toStr = case toStr of
    (VarString str) -> str
    _ -> ""

simplifyAssignementOperator :: Cpt -> Parser Cpt
simplifyAssignementOperator toAssign = Parser $ \op -> case op of
    "=" -> Just(VarList([toAssign]++[VarString "="]), "")
    "-=" -> Just(VarList([toAssign]++[VarString "="]++[toAssign]++[VarString "-"]), "")
    "*=" -> Just(VarList([toAssign]++[VarString "="]++[toAssign]++[VarString "*"]), "")
    "+=" -> Just(VarList([toAssign]++[VarString "="]++[toAssign]++[VarString "+"]), "")
    "/=" -> Just(VarList([toAssign]++[VarString "="]++[toAssign]++[VarString "/"]), "")
    "%=" -> Just(VarList([toAssign]++[VarString "="]++[toAssign]++[VarString "%"]), "")
    _ -> Nothing

parseAssignementOperator :: Parser Cpt
parseAssignementOperator = Parser $ \str -> case runParser parseMyString str of
    Nothing -> Nothing
    Just(var, xs) -> case runParser (parseEmptySpaces *> parseMyString <* parseEmptySpaces) xs of
        Nothing -> Nothing
        Just(ass, ys) -> case runParser (simplifyAssignementOperator var) (cptStringToString ass) of
            Nothing -> Nothing
            Just(res, _) -> case runParser (parseExpressionManipulation) ys of
                Nothing -> Nothing
                Just(res', (';':rest)) -> Just(VarList([res]++[res']), rest)
                Just(res', rest) -> Just(VarList([res]++[res']), rest)

parseInScope :: Parser Cpt
parseInScope = parseEmptySpaces *> (
    parseDeclaration
    <|> parseIf
    <|> parseWhile
    <|> parseAssignementOperator
    <|> parseVariableDeclaration
    <|> parseReturn
    <|> parseParenthesisReturn
    <|> parseFunctionCall
    )

parseVarExpression :: Parser Cpt
parseVarExpression = parseCptInt
    <|> parseFloat
    <|> parseBoolean
    <|> parseFunctionCall
    <|> parseOperator
    <|> parseMyString
    <|> parseParenthesisCondition

parseVarExpression2 :: Parser Cpt
parseVarExpression2 = parseCptInt
    <|> parseFloat
    <|> parseBoolean
    <|> parseFunctionCall
    <|> parseOperator
    <|> parseMyString


parseParenthesisCondition :: Parser Cpt
parseParenthesisCondition = VarList <$> ((\a -> [a]) <$> (parseChar '(' *> parseCondOp2 <* parseChar ')'))

extractCondOp :: Parser Cpt
extractCondOp = ((parseEmptySpaces *> parseOperator) <|> parseOperator <|> (parseEmptySpaces *> parseVarExpression) <|> parseVarExpression)

extractCondOp2 :: Parser Cpt
extractCondOp2 = ((parseEmptySpaces *> parseOperator) <|> parseOperator <|> (parseEmptySpaces *> parseVarExpression2) <|> parseVarExpression2)

parseCondOp2 :: Parser Cpt
parseCondOp2 = VarList <$> ((++) <$> (\a -> [a]) <$> (parseVarExpression2) <*> (parseMany (extractCondOp)))

parseCondOp :: Parser Cpt
parseCondOp = VarList <$> ((++) <$> (\a -> [a]) <$> (parseVarExpression) <*> (parseMany (extractCondOp)))

parseConditionsOperations :: Parser Cpt
parseConditionsOperations = VarList <$> ((\str -> [str]) <$> (parseChar '(' *> parseCondOp <* parseChar ')'))

parseIf :: Parser Cpt
parseIf = Parser $ \input -> case runParser (((++) <$> (\str -> [str]) <$> (parseWord "if" <* parseEmptySpaces) <*> ((\a -> [a]) <$> parseConditionsOperations))) input of
    Nothing -> Nothing
    Just(res, rest) -> case runParser (((parseEmptySpaces *> parseChar '{') <|> parseChar '{') *> (parseMany parseInScope) <* ((parseEmptySpaces <* parseChar '}'))) rest of
        Nothing -> Nothing
        Just(res', rest') -> case runParser ((++) <$> (\a -> [a]) <$> (parseEmptySpaces *> parseWord "else" <* parseEmptySpaces <* parseChar '{') <*> (parseMany parseInScope) <* ((parseEmptySpaces <* parseChar '}'))) rest' of
            Nothing -> Just(VarList(res++res'), rest')
            Just(end, rrest) -> Just(VarList(res++res'++end), rrest)
        
        -- Just(VarList (res), rest)

parseWhile :: Parser Cpt
parseWhile = Parser $ \input -> case runParser (((++) <$> (\str -> [str]) <$> (parseWord "while" <* parseEmptySpaces) <*> ((\a -> [a]) <$> parseConditionsOperations))) input of
    Nothing -> Nothing
    Just(res, rest) -> case runParser (((parseEmptySpaces *> parseChar '{') <|> parseChar '{') *> (parseMany parseInScope) <* ((parseEmptySpaces <* parseChar '}'))) rest of
        Nothing -> Nothing
        Just(res', rest') -> Just(VarList(res++res'), rest')

parseVariableDeclaration :: Parser Cpt
parseVariableDeclaration = VarList <$> ((++) <$> (\str -> [str]) <$> (parseWord "var" <* parseEmptySpaces) <*> ((++) <$> (\a -> [a]) <$> (parseDeclarationTypes <* parseEmptySpaces) <*> ((++) <$> (\a -> [a]) <$> (parseMyString <* parseEmptySpaces) <*> ((\a -> [a]) <$> parseAssignation))))

parseArgumentVariable :: Parser Cpt
parseArgumentVariable = VarList <$> ((++) <$> (\a -> [a]) <$> (parseWord "var" <* parseEmptySpaces) <*> ((++) <$> (\a -> [a]) <$> (parseDeclarationTypes <* parseEmptySpaces) <*> ((\a -> [a]) <$> (parseMyString))))

parseSingleArg :: Parser Cpt
parseSingleArg = VarList <$> ((\a -> [a]) <$> parseArgumentVariable <* parseChar ')')
    
parseMultipleArgs :: Parser Cpt
parseMultipleArgs = VarList <$> ((++) <$> (\a -> [a]) <$> parseArgumentVariable <*> (parseMany (parseChar ',' *> parseEmptySpaces *> parseArgumentVariable)) <* parseChar ')')

parseFunctionDeclaration :: Parser Cpt
parseFunctionDeclaration = VarList <$> ((++) <$> (\str -> [str]) <$> (parseWord "function" <* parseEmptySpaces) <*> ((++) <$> (\a -> [a]) <$> (parseDeclarationTypes <* parseEmptySpaces) <*> ((++) <$> (\a -> [a]) <$> (parseMyString <* parseChar '(') <*> ((\a -> [a]) <$> (parseSingleArg <|> parseMultipleArgs <|> (parseWord "void" <* parseChar ')'))))))

parsePossibleParameter :: Parser Cpt
parsePossibleParameter = parseCptInt
    <|> parseFloat
    <|> parseBoolean
    <|> parseStringDeclaration
    <|> parseCharDeclaration
    <|> parseMyString

parseMultipleParameter :: Parser Cpt
parseMultipleParameter = VarList <$> ((++) <$> (\a -> [a]) <$> parsePossibleParameter <*> (parseMany (parseChar ',' *> parseEmptySpaces *> parsePossibleParameter) <* parseChar ')'))

parseParameter :: Parser Cpt
parseParameter = VarList <$> ((\a -> [a]) <$> (parseChar '(' *> (((parseMultipleParameter) <* parseChar ')') <|> (parseWord "void" <* parseChar ')'))))

parseFunctionCall :: Parser Cpt
parseFunctionCall = VarList <$> ((++) <$> (\a -> [a]) <$>  (parseChar '(' *> parseMyString) <*> ((\a -> [a]) <$> (parseParameter <* parseChar ';')))

parseFunctionBody :: Parser Cpt
parseFunctionBody = VarList <$> ((\a -> [a]) <$> parseChar '{' *> (parseMany parseInScope) <* (parseEmptySpaces <* parseChar '}'))

parseFunction :: Parser Cpt
parseFunction = VarList <$> ((++) <$> (\a -> [a]) <$> parseFunctionDeclaration <*> ((\a -> [a]) <$> (parseEmptySpaces *> parseFunctionBody)))

parseAll :: Parser Cpt
parseAll = (parseEmptySpaces *> (parseFunction)) 
    <|> (parseFunction)
    <|> parseInScope

parseInstruction :: Parser Cpt
parseInstruction = VarList <$> (parseMany parseAll)
