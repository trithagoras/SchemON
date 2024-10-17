module Lexer where
import Data.Char (isAlpha, isAlphaNum)

data SyntaxError = UnexpectedToken deriving (Show)

data TokenKind
    -- single character tokens
    = Colon | Comma | LeftSquare | RightSquare | LeftBrace | RightBrace | Null
    -- types
    | Int | Float | Bool | Char | Str
    -- others
    | Identifier String | EOF | NoOp | Ignore | NewLine    -- NoOp represents a bad token; Ignore is something to be ignored, e.g. whitespace
    deriving (Show)

-- Kind, Line #, Position in line, lexume, Error
data Token = Token TokenKind Int Int String (Maybe SyntaxError) deriving (Show)

scanTokens :: String -> [Token]
scanTokens source = scanTokens' source 0 0 0 []

--          source -> line -> start -> current -> acc -> tokens
scanTokens' :: String -> Int -> Int -> Int -> [Token] -> [Token]
scanTokens' source line start current acc
    | isAtEnd source current = acc ++ [Token EOF line start "" Nothing]
    | otherwise = case scanToken source line start current of
        Token EOF l s w Nothing -> acc ++ [Token EOF l s w Nothing]
        Token Ignore l s w Nothing -> scanTokens' source line (start + length w) (start + length w) acc
        Token NewLine l s w Nothing -> scanTokens' source (line + 1) (start + length w) (start + length w) acc
        Token t l s w e -> scanTokens' source line (start + length w) (start + length w) (acc ++ [Token t l s w e])

scanToken :: String -> Int -> Int -> Int -> Token
scanToken source line start current =
    let word = slice start current source in
    case word of
        "" -> Token EOF line start word Nothing
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
            | isAlpha c || c == '_' -> identifier source line start current
            | otherwise -> Token NoOp line start word (Just UnexpectedToken)

identifier :: String -> Int -> Int -> Int -> Token
identifier source line start current =
    let word = slice start current source in 
    case peekNext source current of
        Nothing -> Token (Identifier word) line start word Nothing
        Just c
            | isAlphaNum c || c == '_' -> identifier source line start (current + 1)
            | otherwise -> matchIdentifier (slice start current source) line start

matchIdentifier :: String -> Int -> Int -> Token
matchIdentifier word line start = case word of
    "int" -> Token Int line start word Nothing
    "float" -> Token Float line start word Nothing
    "str" -> Token Str line start word Nothing
    "bool" -> Token Bool line start word Nothing
    "char" -> Token Char line start word Nothing
    _ -> Token (Identifier word) line start word Nothing

isAtEnd :: [a] -> Int -> Bool
isAtEnd source current = current >= length source

peekNext :: String -> Int -> Maybe Char
peekNext source index
    | (index + 1) >= length source = Nothing
    | otherwise = Just $ source !! (index + 1)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

program = "person: {name: str, age: int}"