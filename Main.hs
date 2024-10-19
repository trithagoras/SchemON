module Main where
import Schemon
import Targets
import System.IO
import System.Environment (getArgs)

main = do
    args <- getArgs
    case args of
        [targetName, fileName] -> main2 fileName targetName Nothing
        [targetName, fileName, out] -> main2 fileName targetName $ Just out
        _ -> putStrLn "Usage: schemon <target> <input-file-name> [output-file-name]"

main2 :: FilePath -> String -> Maybe String -> IO ()
main2 fileName targetName maybeOut = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let outputFile = case maybeOut of
            Nothing -> "out" ++ outputExtension targetName
            Just out -> out
    case targetName of
        "cs"     -> processFile contents encodeCSharp outputFile
        "son"    -> processFile contents encodeSchemON outputFile
        "ts"    -> processFile contents encodeTypeScript outputFile
        _        -> putStrLn $ "Invalid target name. Available targets are:\n" ++ unlines availableTargets

outputExtension :: String -> String
outputExtension "cs"     = ".cs"
outputExtension "son"    = ".son"
outputExtension "ts"     = ".ts"
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

encodeTypeScript :: Program TypeScriptEncoder -> String
encodeTypeScript = encode

availableTargets :: [String]
availableTargets = ["C# (cs)", "SchemON (son)", "TypeScript (ts)"]