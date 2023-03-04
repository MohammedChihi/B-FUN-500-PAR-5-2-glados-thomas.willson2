module Cpt (Cpt(..)) where

import Data.Maybe()

data Cpt = VarInt Int
          | VarString String
          | VarList [Cpt]
          | VarChar Char
          | VarBool Bool
          | VarFloat Float
          deriving (Eq, Ord, Show)

-- getSymbol :: Cpt -> Maybe String
-- getSymbol (VarString symbol) = Just symbol
-- getSymbol _ = Nothing

-- getInt :: Cpt -> Maybe Int
-- getInt (VarInt i) = Just i
-- getInt _ = Nothing

-- getList :: Cpt -> Maybe [Cpt]
-- getList (VarList lst) = Just lst
-- getList _ = Nothing

-- '>>=' symbol meaning:  takes a function, maps it over an instance of a monad and flattens the result
-- monad meaning:         encapsulation of one or many objects of same types
-- printTree :: Cpt -> Maybe String
-- printTree cpt = case cpt of
--   (VarInt i) -> getInt cpt >>= (\x -> Just ("a Number " ++ show x))
--   (VarString s) -> getSymbol cpt >>= (\x -> Just ("a Symbol " ++ show x))
--   (VarList lst) -> getList cpt >>= (\x -> case x of
--     (VarString h:xs) -> Just $ "a List with a Symbol '" ++ h ++ "'" ++ (concatMap (\x -> " , " ++ (fromMaybe "" (printTree x))) xs)
--     _ -> Nothing)

-- Test of printTree:

-- a = VarList [ VarString "define", VarString "x", CptInt 5 ]
-- b = VarString "x"
-- c = VarList [ VarString "define", VarString "y", VarList [ VarString "+", VarInt 4, VarString "x"]]
-- printTree a
-- printTree b
-- printTree c
