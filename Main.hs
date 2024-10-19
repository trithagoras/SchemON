module Main where
import Schemon
import Targets
import System.IO

main = do
    handle <- openFile "test.son" ReadMode
    contents <- hGetContents handle
    case decode contents of
        Left s -> putStrLn $ "Syntactic error: " ++ s
        Right p -> case visit p of
            Left s -> putStrLn $ "Semantics error: " ++ s
            Right () -> putStrLn $ encode (p :: Program CSharpEncoder)