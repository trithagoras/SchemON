module Lexer where
import Data.Char (isAlpha)

data SyntaxError = UnexpectedToken;

data TokenKind
    -- single character tokens
    = Colon | Comma | LeftSquare | RightSquare | LeftBrace | RightBrace | Null
    -- types
    | Int | Float | Bool | Char | Str
    -- others
    | Identifier String | EOF | NoOp    -- NoOp represents a bad token

-- Kind, Lexeme, Line #, Position in line
data Token = Token TokenKind String Int Int (Maybe SyntaxError)

-- tail recursive
scanTokens :: String -> [Token]
scanTokens input = scanTokens' input []

scanTokens' :: String -> [Token] -> [Token]
scanTokens' "" acc = acc ++ [Token EOF "" 0 0 Nothing]

scanToken :: String -> Int -> Int -> Token
scanToken source line col = case source of
    "" -> Token EOF "" line col Nothing
    "," -> Token Comma "" line col Nothing
    ":" -> Token Colon "" line col Nothing
    "[" -> Token LeftSquare "" line col Nothing
    "]" -> Token RightSquare "" line col Nothing
    "{" -> Token LeftBrace "" line col Nothing
    "}" -> Token RightBrace "" line col Nothing
    "?" -> Token Null "" line col Nothing
    "int" -> Token Int source line col Nothing
    "float" -> Token Float source line col Nothing
    "str" -> Token Str source line col Nothing
    "bool" -> Token Bool source line col Nothing
    "char" -> Token Char source line col Nothing
    (c:_)
        | isAlpha c || c == '_' -> Token (Identifier source) source line col (Just UnexpectedToken)
        | otherwise -> Token NoOp "" line col (Just UnexpectedToken)