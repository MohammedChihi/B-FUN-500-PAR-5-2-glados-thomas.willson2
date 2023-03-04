import Test.HUnit
import ParserBootstrapTests
import ParserTests
-- import AstTests

main :: IO ()
main = do
    counts1 <- runTestTT testParserBootstrapTests
    putStrLn $ "Counts for testParserBootstrapTests: " ++ show counts1
    counts2 <- runTestTT testParserFunctions
    putStrLn $ "Counts for testParserFunctions: " ++ show counts2
    -- counts2 <- runTestTT testAst
    -- putStrLn $ "Counts for testAst: " ++ show counts2
