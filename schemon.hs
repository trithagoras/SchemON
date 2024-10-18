import Data.Char (isAlpha, isAlphaNum)
import System.IO (openFile, IOMode (ReadMode), hGetContents)

----------------------------------- TYPES -----------------------------------

-- AST
data Program a = Message [SPair a] EOF deriving (Show)
data SType a = TStr | TInt | TFloat | TBool | TChar | TList (SType a) | TObj [SPair a] | TNullable (SType a) | TCustom Identifier deriving (Show)
data SPair a = SPair Identifier (SType a) deriving (Show)
newtype Identifier = Identifier String deriving (Show, Eq)
data EOF = EOF deriving (Show)

-- Tokens
data SyntaxError = UnexpectedToken deriving (Show)
data TokenKind
    = Colon | Comma | LeftSquare | RightSquare | LeftBrace | RightBrace | Null
    | Int | Float | Bool | Char | Str
    | Identifier' String | EOF' | NoOp | Ignore | NewLine    -- NoOp represents a bad token; Ignore is something to be ignored, e.g. whitespace
    deriving (Show, Eq)

-- Kind, Line #, Position in line, lexume, Error
data Token = Token TokenKind Int Int String (Maybe SyntaxError)

instance Show Token where
    show (Token kind line col repr err) = repr ++ " at line " ++ show line ++ ":" ++ show col

----------------------------------- DECODER -----------------------------------

decode :: String -> Either String (Program a)
decode source = do
    let stream = scanTokens source
    case program stream of
        Left s -> Left s
        Right (p, c) -> return p

----------------------------------- ENCODER -----------------------------------

class Encoder a where
    encode :: Program a -> String

data SchemONEncoder = SchemONEncoder
instance Encoder SchemONEncoder where
    encode (Message pairs EOF) = encodePairs pairs

encodePair :: SPair a -> String
encodePair (SPair ident t) = encodeIdentifier ident ++ ": " ++ encodeType t

encodePairs :: [SPair a] -> String
encodePairs [] = ""
encodePairs [x] = encodePair x
encodePairs (x:xs) = encodePair x ++ ", " ++ encodePairs xs

encodeType :: SType a -> String
encodeType t = case t of
    TStr -> "str"
    TInt -> "int"
    TFloat -> "float"
    TBool -> "bool"
    TChar -> "char"
    TList a -> "[" ++ encodeType a ++ "]"
    TObj a -> "{" ++ encodePairs a ++ "}"
    TNullable a -> encodeType a ++ "?"
    TCustom ident -> encodeIdentifier ident

encodeIdentifier :: Identifier -> String
encodeIdentifier (Identifier s) = s

----------------------------------- LEXER -----------------------------------

scanTokens :: String -> [Token]
scanTokens source = scanTokens' source 1 0 0 []

--          source -> line -> start -> current -> acc -> tokens
scanTokens' :: String -> Int -> Int -> Int -> [Token] -> [Token]
scanTokens' source line start current acc
    | isAtEnd source current = acc ++ [Token EOF' line start "" Nothing]
    | otherwise = case scanToken source line start current of
        Token EOF' l s w Nothing -> acc ++ [Token EOF' l s w Nothing]
        Token Ignore l s w Nothing -> scanTokens' source line (start + length w) (start + length w) acc
        Token NewLine l s w Nothing -> scanTokens' source (line + 1) (start + length w) (start + length w) acc
        Token t l s w e -> scanTokens' source line (start + length w) (start + length w) (acc ++ [Token t l s w e])

scanToken :: String -> Int -> Int -> Int -> Token
scanToken source line start current =
    let word = slice start current source in
    case word of
        "" -> Token EOF' line start word Nothing
        " " -> Token Ignore line start word Nothing
        "\n" -> Token NewLine line start word Nothing
        "," -> Token Comma line start word Nothing
        ":" -> Token Colon line start word Nothing
        "[" -> Token LeftSquare line start word Nothing
        "]" -> Token RightSquare line start word Nothing
        "{" -> Token LeftBrace line start word Nothing
        "}" -> Token RightBrace line start word Nothing
        "?" -> Token Null line start word Nothing
        (c:_)
            | isAlpha c || c == '_' -> scanIdentifier source line start current
            | otherwise -> Token NoOp line start word (Just UnexpectedToken)

scanIdentifier :: String -> Int -> Int -> Int -> Token
scanIdentifier source line start current =
    let word = slice start current source in 
    case peekNext source current of
        Nothing -> Token (Identifier' word) line start word Nothing
        Just c
            | isAlphaNum c || c == '_' -> scanIdentifier source line start (current + 1)
            | otherwise -> matchIdentifier' (slice start current source) line start

matchIdentifier' :: String -> Int -> Int -> Token
matchIdentifier' word line start = case word of
    "int" -> Token Int line start word Nothing
    "float" -> Token Float line start word Nothing
    "str" -> Token Str line start word Nothing
    "bool" -> Token Bool line start word Nothing
    "char" -> Token Char line start word Nothing
    _ -> Token (Identifier' word) line start word Nothing

isAtEnd :: [a] -> Int -> Bool
isAtEnd source current = current >= length source

peekNext :: [a] -> Int -> Maybe a
peekNext source index
    | (index + 1) >= length source = Nothing
    | otherwise = Just $ source !! (index + 1)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

----------------------------------- PARSER -----------------------------------

program :: [Token] -> Either String (Program a, Int)
program stream = do
    (ps, current) <- pairs stream 0
    current <- expect stream current EOF'
    return (Message ps EOF, current)

pairs :: [Token] -> Int -> Either String ([SPair a], Int)
pairs stream current = do
    (p, current) <- pair stream current
    case peekAt stream current of
        Right (Token Comma _ _ _ _) -> do
            current <- expect stream current Comma
            (ps, current) <- pairs stream current
            return (p:ps, current)
        _ -> return ([p], current)

pair :: [Token] -> Int -> Either String (SPair a, Int)
pair stream current = do
    (ident, current) <- identifier stream current
    current <- expect stream current Colon
    (t, current) <- type' stream current
    return (SPair ident t, current)

listType :: [Token] -> Int -> Either String (SType a, Int)
listType stream current = do
    current <- expect stream current LeftSquare
    (t, current) <- type' stream current
    current <- expect stream current RightSquare
    return (TList t, current)

objType :: [Token] -> Int -> Either String (SType a, Int)
objType stream current = do
    current <- expect stream current LeftBrace
    (ps, current) <- pairs stream current
    current <- expect stream current RightBrace
    return (TObj ps, current)


type' :: [Token] -> Int -> Either String (SType a, Int)
type' stream current = do
    token <- peekAt stream current
    (t, current) <- case token of
        Token Int _ _ _ _ -> Right (TInt, current + 1)
        Token Float _ _ _ _ -> Right (TFloat, current + 1)
        Token Char _ _ _ _ -> Right (TChar, current + 1)
        Token Bool _ _ _ _ -> Right (TBool, current + 1)
        Token Str _ _ _ _ -> Right (TStr, current + 1)
        Token (Identifier' s) _ _ _ _ -> Right (TCustom (Identifier s), current + 1)
        Token LeftSquare _ _ _ _ -> listType stream current
        Token LeftBrace _ _ _ _ -> objType stream current
        _ -> Left $ "Expected a type but received " ++ show token
    let nullT = peekAt stream current
    case nullT of
        Right (Token Null _ _ _ _) -> Right (TNullable t, current + 1)
        _ -> Right (t, current)

-- expect :: stream -> current -> expectedToken -> newCurrent
expect :: [Token] -> Int -> TokenKind -> Either String Int
expect stream current tokenKind = do
    Token k l c r e <- peekAt stream current
    if k == tokenKind then Right (current + 1) else Left $ "Expected a token of kind " ++ show tokenKind ++ " but received " ++ show (Token k l c r e)


identifier :: [Token] -> Int -> Either String (Identifier, Int)
identifier stream current = do
    t <- peekAt stream current
    case t of
        Token (Identifier' s) _ _ _ _ -> Right (Identifier s, current + 1)
        _ -> Left $ "Expected Identifier, received " ++ show t

peekAt :: [a] -> Int -> Either String a
peekAt stream current
    | isAtEnd stream current = Left $ "Out of bounds: " ++ show current ++ " is >= " ++ show (length stream)
    | current < 0 = Left $ "Out of bounds: " ++ show current ++ " is <= 0"
    | otherwise = Right $ stream !! current


----------------------------------- SEMANTICS -----------------------------------

-- a newly constructed AST should be verified via semantic checking
-- an example semantic rule is that top-level messages should only be of object type.

isMessage :: SPair a -> Either String ()
isMessage (SPair _ (TObj _)) = Right ()
isMessage p = Left $ "Top level messages must be of type 'object'. Encountered: " ++ show p

isMessages :: [SPair a] -> Either String ()
isMessages = mapM_ isMessage

visit :: Program a -> Either String ()
visit prog = visitProgram prog []

visitProgram :: Program a -> [Identifier] -> Either String ()
visitProgram (Message ps EOF) idents = do
    _ <- isMessages ps
    _ <- visitPairs ps idents
    return ()

visitPairs :: [SPair a] -> [Identifier] -> Either String ()
visitPairs [] idents = Right ()
visitPairs (p:ps) idents = do
    _ <- visitPair p idents
    visitPairs ps idents

visitPair :: SPair a -> [Identifier] -> Either String ()
visitPair (SPair ident t) idents = do
    _ <- visitIdentifier ident idents
    _ <- visitType t idents
    return ()
    
visitIdentifier :: Identifier -> [Identifier] -> Either String ()
visitIdentifier (Identifier s) idents = do
    -- TODO: need to check that this identifier is not already in scope
    return ()

visitType :: SType a -> [Identifier] -> Either String ()
visitType (TCustom ident) idents = do
    _ <- visitIdentifier ident idents
    if ident `elem` idents then Right () else Left $ "Identifier " ++ show ident ++ " has not been defined before trying to use it as a type."
visitType (TObj inner) idents = visitPairs inner idents
visitType (TList inner) idents = visitType inner idents
visitType _ _ = Right ()


----------------------------------- MAIN -----------------------------------

main = do
    handle <- openFile "test.son" ReadMode
    contents <- hGetContents handle
    case decode contents of
        Left s -> putStrLn $ "Syntactic error: " ++ s
        Right p -> case visit p of
            Left s -> putStrLn $ "Semantics error: " ++ s
            Right () -> putStrLn $ encode (p :: Program SchemONEncoder)

