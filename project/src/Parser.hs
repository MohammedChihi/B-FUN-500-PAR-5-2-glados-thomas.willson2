module Parser(
    parseString,
    parseSymbol,
    parseCompute,
    parseTokens,
    isSymbolChar
) where
import DataStructures

-- symbolCharacters est une chaîne de caractères qui représente tous les caractères qui peuvent faire partie d'un symbole en Scheme.
symbolCharacters :: String
symbolCharacters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_!?-+*/%<>#"

-- numberCharacters est une chaîne de caractères qui représente tous les caractères qui peuvent faire partie d'un nombre en Scheme.
numberCharacters :: String
numberCharacters = "0123456789.-"

-- isSymbolChar est une fonction qui prend un caractère en entrée et renvoie True s'il fait partie des caractères permis pour un symbole en Scheme et False sinon.
isSymbolChar :: Char -> Bool
isSymbolChar ch = elem ch symbolCharacters

-- isSymbol est une fonction qui prend une chaîne de caractères en entrée et renvoie True s'elle ne contient que des caractères permis pour un symbole en Scheme et False sinon.
isSymbol :: String -> Bool
isSymbol = all isSymbolChar

-- isNumberChar est une fonction qui prend un caractère en entrée et renvoie True s'il fait partie des caractères permis pour un nombre en Scheme et False sinon.
isNumberChar :: Char -> Bool
isNumberChar ch = elem ch numberCharacters

-- isNumber est une fonction qui prend une chaîne de caractères en entrée et renvoie True s'elle ne contient que des caractères permis pour un nombre en Scheme et False sinon.
isNumber :: String -> Bool
isNumber = all isNumberChar

parseSpaces :: String -> Maybe String
parseSpaces (' ':xs) = Just (xs)
parseSpaces ('\n':xs) = Just (xs)
parseSpaces ('\t':xs) = Just (xs)
parseSpaces _ = Nothing

parseString :: String -> String -> Maybe (String, String)
parseString [] [] = Nothing
parseString [] keep = Just(keep, "")
parseString (x:xs) keep | x == ')' = Just(keep, (x:xs))
    | x == '(' = Just(keep, (x:xs))
    | x == ' ' = Just(keep, xs)
    | x == '\n' = Just(keep, xs)
    | x == '\t' = Just(keep, xs)
    | otherwise = (parseString xs (keep++[x]))

parseSymbol :: String -> Maybe (String, String)
parseSymbol [] = Nothing
parseSymbol (x:xs) | x == '(' || x == ')' = Just([x], xs)
    | otherwise = Nothing

parseCompute :: String -> [String] -> [String]
parseCompute [] lst = lst
parseCompute str lst = case parseSpaces str of
    Just (cleared) -> parseCompute cleared lst
    Nothing -> case parseSymbol str of
        Just (sym, rest) -> parseCompute rest (lst++[sym])
        Nothing -> case parseString str "" of
            Just (parsed, rest) -> parseCompute rest (lst++[parsed])
            Nothing -> parseCompute str lst

-- parseToken est une fonction qui prend une chaîne de caractères en entrée et renvoie une expression Scheme correspondante.
parseToken :: [String] -> (SExpr, [String])
parseToken [] = (NullExpr, [])
parseToken (x:xs)
  | x == "(" = parseList xs
  | x == "#t" = ((BoolExpr True), xs)
  | x == "#f" = ((BoolExpr False), xs)
  | x == "null" = ((NullExpr), xs)
  | isNumber x = ((NumExpr (read x :: Int)), xs)
  | isSymbol x = ((SymbolExpr x), xs)
  | otherwise = (NullExpr, [])

-- parseTokens est une fonction qui prend une liste de chaînes de caractères en entrée et renvoie une liste d'expressions Scheme correspondantes.
parseTokens :: [String] -> [SExpr]
parseTokens [] = []
parseTokens tokens = expr : parseTokens rest
                       where (expr, rest) = parseToken tokens

-- parseList est une fonction qui prend une liste de chaînes de caractères en entrée et renvoie une paire contenant une expression Scheme correspondante et le reste de la liste de chaînes de caractères
parseList :: [String] -> (SExpr, [String])
parseList [] = (NullExpr, [])
parseList tokens@(x:xs)
  | x == ")" = (NullExpr, xs)
  | otherwise = ((Pair expr1 expr2), rest2)
                where (expr1, rest1) = parseToken tokens
                      (expr2, rest2) = parseList rest1
