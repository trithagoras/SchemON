module Main where

-- Base encoding class and built-in SchemaON Encoder
-- TODO: make everything tail recursive!

class Encoder a where
    encode :: Program a -> String

data SchemONEncoder = SchemONEncoder
instance Encoder SchemONEncoder where
    encode EOF = ""
    encode (Message pair enc) = encodePair pair ++ "\n" ++ encode enc

data Program a = Message (SPair a) (Program a) | EOF deriving (Show)

data SType a = TStr | TInt | TFloat | TBool | TChar | TList (SType a) | TObj (ObjRValue a) | TNullable (SType a) | TCustom String deriving (Show)
data SPair a = SPair String (SType a) deriving (Show)

type ObjRValue a = [SPair a]

encodePair :: SPair a -> String
encodePair (SPair name t) = name ++ ": " ++ encodeType t

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
    TCustom s -> s


-- Decoding

-- data DecodeError = SyntaxError String | SemanticsError String
-- type DecodeResult a = Either [DecodeError] (Program a)

-- strip :: String -> String
-- strip = filter (/= ' ') . filter (/= '\n') . filter (/= '\t')

-- decode :: String -> DecodeResult a
-- decode program = do
--     let s = strip program
--     return $ Message (SPair "root" TInt) EOF

-- Test program

program :: Program a
packet :: SPair a
translate :: SPair a

program = Message packet $ Message translate EOF
packet = SPair "packet" (TObj [SPair "id" TInt])
translate = SPair "translate" (TObj [SPair "packet" (TCustom "packet"), SPair "dx" TFloat, SPair "dy" TFloat, SPair "collisionId" (TNullable TInt)])
-- packet: {id: int}
-- translate: {packet: packet, dx: float, dy: float, collisionId: int?}

main = do
    putStrLn $ encode (program :: Program SchemONEncoder)