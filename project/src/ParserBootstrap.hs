module ParserBootstrap (
    module ParserBootstrap
) where 

import Control.Applicative

-- Ce code définit un type Parser a, qui représente un analyseur de chaîne de caractères.
-- Le type a une seule méthode, runParser, qui prend une chaîne en entrée et renvoie un Maybe (a, String).
-- Si l'analyse réussit, Just (a, String) sera renvoyé, où a est le résultat de l'analyse et String est la chaîne restante qui n'a pas été analysée.
-- Si l'analyse échoue, Nothing sera renvoyé.
data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

-- Définition de l'instanciation de Functor pour le type Parser
instance Functor Parser where
    fmap fct (Parser a) = Parser $ \input -> case a input of
        Just (res, rest) -> Just ((fct res), rest)
        Nothing -> Nothing

-- Ce code définit une instance Applicative pour le type Parser.
-- La fonction pure prend une valeur a et la place dans un parseur qui retourne toujours cette valeur en utilisant une entrée vide.
-- * Les fonctions <*>, <*, et *> permettent de combiner des parseurs ensemble.
-- Si un parseur ne peut pas être appliqué, il retourne Nothing.
instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    (Parser one) <*> (Parser two) = Parser $ \input -> case one input of
        Just (res, rest) -> case two rest of
            Just (res', rest') -> Just (res res', rest')
            Nothing -> Nothing
        Nothing -> Nothing

-- Ceci définit l'instance Alternative pour le type Parser.
-- Le comportement de l'instance est défini pour les méthodes empty et <|>.
-- empty est une analyseur qui ne réussit jamais et renvoie toujours Nothing.
-- * <|> essaie d'abord d'appliquer l'analyseur one sur l'entrée donnée.
-- Si cela échoue, il essaie alors de l'appliquer sur two.
-- Si l'un des deux réussit, alors le résultat est renvoyé.
-- Si les deux échouent, Nothing est renvoyé.
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser one) <|> (Parser two) = Parser $ \s -> case one s of
            Nothing -> two s
            Just (res, rest)  -> Just (res, rest)

-- Ce code définit une instance Monad pour la classe de données Parser.
-- * La méthode (>>=) permet de concatener deux parseurs en séquence.
-- Le parseur a est exécuté en premier et s'il réussit avec une sortie (x', s'), alors la fonction k est appliquée à x' pour obtenir un nouveau parseur.
-- Enfin, ce nouveau parseur est exécuté avec l'entrée s' pour obtenir le résultat final.
instance Monad Parser where
    Parser a >>= k = Parser $ \input -> do
        (x', s') <- a input
        runParser (k x') s'

-- La fonction runParser est définie pour prendre en entrée une chaîne de caractères str
parseChar :: Char -> Parser Char
parseChar c = Parser {
    runParser = \str -> case str of
        (x:xs) -> if x == c then Just(c, xs) else Nothing
        _ -> Nothing
}

-- La fonction parseAnyChar prend en entrée une chaîne de caractères et renvoie un parseur de caractères.
parseAnyChar :: String -> Parser Char
parseAnyChar "" = Parser {
    runParser = \_ -> Nothing
}
parseAnyChar (x:xs) = Parser {
    runParser = \str -> case (runParser (parseChar x) str) of
        Nothing -> (runParser (parseAnyChar xs) str)
        result -> result
}

-- Cette fonction définit un parseur qui peut utiliser soit "first" soit "second"
-- en fonction de la disponibilité de la sortie de "first". Si la sortie de "first" est Nothing,
-- alors "second" est utilisé et si la sortie de "first" est disponible, celle-ci est utilisée.
parseOr :: Parser a -> Parser a -> Parser a
parseOr first second = Parser {
    runParser = \str -> case runParser first str of
        Nothing -> (runParser second str)
        result -> result
}

-- Cette fonction deux parseurs a et b et renvoie un parseur qui parse un couple (a,b).
-- La fonction runParser définit la logique de parsing pour ce parseur combiné : il d'abord exécute le parseur first sur la chaîne d'entrée str
-- puis utilise le résultat pour exécuter le parseur second sur la chaîne restante.
-- Si les deux parsages sont réussis, alors le parseur combiné renvoie un Just avec le résultat des parsages (en tant que couple) et la chaîne restante.
parseAnd :: Parser a -> Parser b -> Parser (a , b)
parseAnd first second = Parser {
    runParser = \str -> (runParser first str) >>=
        \(resx, xs) -> (runParser second xs) >>=
            \(resy, ys) -> (Just((resx, resy), ys))
}

-- Ce code définit une fonction parseAndWith qui prend en entrée une fonction f, ainsi que des Parser a et Parser b.
-- Il retourne un nouveau Parser c qui combine les résultats des deux entrées en utilisant la fonction f pour combiner les résultats.
-- La fonction runParser effectue cette combinaison en utilisant parseAnd pour parser les entrées first et second, puis en utilisant f pour combiner les résultats,
-- et en retournant le résultat combiné dans un Just avec le reste de la chaîne en entrée.
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f first second = Parser {
    runParser = \str -> (runParser (parseAnd first second) str) >>=
        \((x, y), xs) -> Just(f x y, xs)
}

-- Le code définit une fonction parseMany qui prend en entrée un Parser a et retourne un Parser [a].
-- La fonction runParser est définie pour retourner soit une liste vide avec une chaîne vide si la chaîne d'entrée est vide,
-- soit un résultat obtenu à partir de l'application du Parser p sur la chaîne d'entrée,
-- soit une liste vide avec la chaîne d'entrée inchangée si l'application de Parser p sur la chaîne d'entrée échoue.
-- Dans le cas où l'application réussie, le résultat est concaténé avec les résultats obtenus par répétition de l'application de Parser p sur le reste de la chaîne d'entrée.
parseMany :: Parser a -> Parser [a]
parseMany p = Parser {
    runParser = \str -> case str of
        "" -> Just([], "")
        _ ->  case runParser p str of
            Nothing -> Just([], str)
            Just(x, rest) -> case runParser (parseMany p) rest of
                Nothing -> Just([x], rest)
                Just(xs, rest') -> Just(x:xs, rest')
}

-- ! can use parseAnd
-- Ce code définit une fonction parseSome qui prend en entrée un parseur p et qui retourne un parseur pour une liste d'éléments de type a.
-- La fonction runParser définit comment ce parseur fonctionne : il utilise d'abord le parseur p sur la chaîne d'entrée str,
-- puis utilise le parseur parseMany p sur le reste de la chaîne pour obtenir une liste d'éléments de type a.
-- Enfin, il concatène le premier élément avec cette liste et retourne le résultat sous forme de Just avec la liste complète et le reste de la chaîne d'entrée.
parseSome :: Parser a -> Parser [a]
parseSome p = Parser {
    runParser = \str -> (runParser p str) >>=
        \(x, rest) -> (runParser (parseMany p) rest) >>=
            \(xs, rest') -> Just(x:xs, rest')
}

-- Ce code définit un parseur d'entier non signé (parseUInt).
-- Il utilise un parseur de caractères pour capturer une séquence de caractères de chiffres de 0 à 9 (parseSome $ parseAnyChar ['0'..'9']).
-- Ensuite, la fonction read est utilisée pour convertir la chaîne en entier.
-- Si la chaîne est vide ou si la conversion échoue, le parseur retourne Nothing. Sinon, il retourne le résultat converti dans une valeur Just.
parseUInt :: Parser Int
parseUInt = Parser {
    runParser = \str -> case str of
        "" -> Nothing
        res -> case (runParser ((\s -> read s :: Int) <$> (parseSome $ parseAnyChar  ['0'..'9'])) res) of
            Nothing -> Nothing
            result -> result
}

parseCheckIsFloat :: String -> Bool
parseCheckIsFloat = \str -> case runParser (parseSome $ parseAnyChar ['0'..'9']) str of
    Nothing -> False
    Just(_, ('.':_)) -> True
    Just(_, _) -> False

-- Ce code définit un analyseur (parseInt) pour des entiers signés.
-- L'analyseur utilise parseUInt pour analyser les entiers non signés et gère le signe négatif en vérifiant s'il y a un - au début de la chaîne en entrée.
-- Si oui, il retourne le négatif de la valeur analysée par parseUInt, sinon il retourne directement la valeur analysée par parseUInt.
parseInt :: Parser Int
parseInt = Parser {
    runParser = \str -> case str of
        "" -> Nothing
        _ -> case parseCheckIsFloat str of
            True -> Nothing
            False -> case (runParser (parseChar '-') str) of
                Nothing -> runParser parseUInt str
                Just(_, xs) -> runParser parseUInt xs >>=
                    \(res, ys) -> Just(-res, ys)
}

-- "parseList" est un parseur pour parser une liste d'éléments.
-- Il utilise d'autres parseurs pour parser les éléments individuels ainsi que les séparateurs.
-- Il commence par parser le caractère "(", puis utilise "parseSome" pour parser une séquence d'éléments séparés par des espaces, puis termine en parseant le caractère ")".
-- Le résultat est une liste d'éléments et la chaîne restante.
parseList :: Parser a -> Parser [a]
parseList p = Parser {
    runParser = \str -> (runParser (parseAnd (parseChar '(') p) str) >>=
        \((_, x), xs) -> (runParser (parseSome (parseAnd (parseSome $ parseChar ' ') p)) xs) >>=
        \(lst, xxs) -> (runParser (parseChar ')') xxs) >>=
        \(_, rest) -> Just(x:(map snd lst), rest)
}
