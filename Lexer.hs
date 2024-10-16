module Lexer where

data TokenKind
    -- single character tokens
    = Colon | Comma | LeftSquare | RightSquare | LeftBrace | RightBrace | Null
    -- types
    | Int | Float | Bool | Char | Str
    -- others
    | Identifier | EOF

-- Kind, Lexeme, Line #, Position in line
data Token = Token TokenKind String Int Int

-- tail recursive
scanTokens :: String -> [Token]
scanTokens input = scanTokens' input []

scanTokens' :: String -> [Token] -> [Token]
scanTokens' "" acc = acc ++ [Token EOF "" 0 0]
