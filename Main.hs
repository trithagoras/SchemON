module Main where
import Schemon
import Targets
import System.IO
import System.Environment (getArgs)

main = do
    args <- getArgs
    case args of
        [fileName, targetName] -> main2 fileName targetName Nothing
        [fileName, targetName, out] -> main2 fileName targetName $ Just out
        _ -> putStrLn "Usage: schemon <input-file-name> <target>"

main2 :: FilePath -> String -> Maybe String -> IO ()
main2 fileName targetName maybeOut = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let outputFile = case maybeOut of
            Nothing -> "out" ++ outputExtension targetName
            Just out -> out ++ outputExtension targetName
    case targetName of
        "cs"     -> processFile contents encodeCSharp outputFile
        "son"    -> processFile contents encodeSchemON outputFile
        _        -> putStrLn $ "Invalid target name. Available targets are:\n" ++ unlines availableTargets

outputExtension :: String -> String
outputExtension "cs"     = ".cs"
outputExtension "son"    = ".son"
outputExtension _        = ""


processFile :: String -> (Program a -> String) -> FilePath -> IO ()
processFile contents encoderFunc outputFile = 
    case decode contents of
        Left s -> putStrLn $ "Syntactic error: " ++ s
        Right p -> case visit p of
            Left s -> putStrLn $ "Semantics error: " ++ s
            Right () -> writeFile outputFile $ encoderFunc p

encodeCSharp :: Program CSharpEncoder -> String
encodeCSharp = encode

encodeSchemON :: Program SchemONEncoder -> String
encodeSchemON = encode

availableTargets :: [String]
availableTargets = ["C# (cs)", "SchemON (son)"]