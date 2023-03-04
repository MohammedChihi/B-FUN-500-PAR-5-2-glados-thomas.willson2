module Lib ( someFunc, toSExprList, startWith, partitions, _foldM ) where
import DataStructures

toSExprList :: SExpr -> [SExpr]
toSExprList NullExpr = []
toSExprList (Pair first rest) = first : toSExprList rest

startWith :: String -> String -> Bool
startWith prefix str = prefix == take (length prefix) str

someFunc :: IO ()
someFunc = putStrLn "hello world"

-- La fonction split qui permet de diviser une chaîne de caractères en sous-chaînes en utilisant un séparateur spécifié.
-- La fonction renvoie une liste de chaînes de caractères.
-- split :: Char -> String -> [String]
-- split sep s = case dropWhile (== sep) s of
--     "" -> []
--     s' -> w : split sep s''
--         where (w, s'') = break (== sep) s'

-- La fonction partition permet de diviser une chaîne de caractères en deux parties en utilisant les parenthèses comme séparateurs.
-- La première partie comprend toutes les parenthèses ouvertes et fermées jusqu'à la fermeture correspondante,
--      et la seconde partie comprend le reste de la chaîne.
partition :: String -> (String, String)
partition [] = ([], [])
partition ('(':xs) = let (open, close) = partition' 1 xs in ('(':open, close)
  where partition' :: Int -> String -> (String, String)
        partition' 0 s = ([], s)
        partition' n ('(':xs') = let (open, close) = partition' (n+1) xs' in ('(':open, close)
        partition' n (')':ys) = let (open, close) = partition' (n-1) ys in (')':open, close)
        partition' n (x:ys') = let (open, close) = partition' n ys' in (x:open, close)
partition (')':xs) = ([], xs)
partition (x:xs) = let (open, close) = partition xs in (x:open, close)

-- La fonction partitions utilise la fonction partition pour diviser une chaîne de caractères en plusieurs
--      sous-chaînes en utilisant les parenthèses comme séparateurs.
-- La fonction renvoie une liste de chaînes de caractères.
partitions :: String -> [String]
partitions s = case partition s of
    ([], []) -> []
    (open, close) -> open : partitions close

_foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
_foldM _ z [] = return z
_foldM f z (x:xs) = do
    z' <- f z x
    _foldM f z' xs
