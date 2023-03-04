module Main (main) where

import System.Environment (getArgs)
import System.IO
import BuiltIn
import DataStructures
import Parser
import EvalAbstractSyntaxTree
import InterpretAst
import Lib

readWithPrompt :: Env -> IO ()
readWithPrompt env =
  do putStr "> "
     hFlush stdout
     eof <- isEOF
     if eof
       then return ()
       else do
        line <- getLine
        let tokens = parseCompute line []
        -- putStrLn $ "Tokens: " ++ show tokens ++ "\n"
        let parsed = parseTokens tokens
        -- putStrLn $ "Parsed: " ++ show parsed ++ "\n"
        let interpret = interpretAST parsed
        -- putStrLn $ "Interpret: " ++ show interpret ++ "\n"
        let (newEnv, result) = evalAST env interpret
        let newResult = "" ++ show result
        if newResult == "FP" then
            if startWith "+" line || startWith "-" line || startWith "*" line || startWith "div" line || startWith "mod" line || startWith "eq?" line
                then putStrLn $ "#<procedure " ++ takeWhile isSymbolChar line ++ ">"
                else readWithPrompt newEnv
        else if startWith "(define" line
                then readWithPrompt newEnv
                else putStrLn $ show result
        -- putStrLn $ show result
        readWithPrompt newEnv

processFile :: Env -> String -> IO Env
processFile env str = do
    let tokens = parseCompute str []
    -- putStrLn $ "Tokens: " ++ show tokens ++ "\n"
    let parsed = parseTokens tokens
    -- putStrLn $ "Parsed: " ++ show parsed ++ "\n"
    let interpret = interpretAST parsed
    -- putStrLn $ "Interpret: " ++ show interpret ++ "\n"
    let (newEnv, result) = evalAST env interpret
    let newResult = "" ++ show result
    if newResult == "FP" then
        if startWith "+" str || startWith "-" str || startWith "*" str || startWith "div" str || startWith "mod" str || startWith "eq?" str
            then putStrLn $ "#<procedure " ++ takeWhile isSymbolChar str ++ ">"
            else pure ()
    else if startWith "(define" str
            then pure ()
            else putStrLn $ show result
    -- putStrLn $ show result
    return newEnv

main :: IO ()
main = do
    args <- getArgs
    env <- initBuiltIn
    case args of
        ["-p"] -> do
            readWithPrompt env
        [file] -> do
            content <- readFile file
            let strings = partitions content
            _ <- _foldM (\e s -> processFile e s) env strings
            return ()
        _ -> putStrLn "Invalid arguments. Usage: exec [-p | file]"
    -- print(runParser (parseWord "if") "if (aaaaa)")
    -- print(runParser parseMyString "this is a test")
    -- print(runParser parseFloat "12.12 afaefezf")
    -- print(runParser parseFloat "dafefezf")
    -- print(tmpCptTest "if" "if (aaaaa)")
    
