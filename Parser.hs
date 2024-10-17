-- TODO: use modules
-- AST
data Program a = Message (SPair a) (Program a) | EOF deriving (Show)
data SType a = TStr | TInt | TFloat | TBool | TChar | TList (SType a) | TObj (ObjRValue a) | TNullable (SType a) | TCustom String deriving (Show)
data SPair a = SPair String (SType a) deriving (Show)
type ObjRValue a = [SPair a]


-- Lexer
data SyntaxError = UnexpectedToken deriving (Show)
data TokenKind
    -- single character tokens
    = Colon | Comma | LeftSquare | RightSquare | LeftBrace | RightBrace | Null
    -- types
    | Int | Float | Bool | Char | Str
    -- others
    | Identifier String | EOF' | NoOp | Ignore | NewLine    -- NoOp represents a bad token; Ignore is something to be ignored, e.g. whitespace
    deriving (Show)

-- Kind, Line #, Position in line, lexume, Error
data Token = Token TokenKind Int Int String (Maybe SyntaxError) deriving (Show)


-- parser
-- parseTokenStream :: [Token] -> Int -> Program a
-- parseTokenStream stream idx
--     | 

parseType (Token Int _ _ _ _) = TInt
parseType (Token Float _ _ _ _) = TFloat
-- ...

parsePair :: [Token] -> SPair a
parsePair [Token (Identifier s) _ _ _ _, t] = SPair s (parseType t)


isAtEnd :: [a] -> Int -> Bool
isAtEnd source current = current >= length source