module Targets where
import Schemon
import Data.Char

----------------------------------- SchemON Encoder -----------------------------------

data SchemONEncoder = SchemONEncoder
instance Encoder SchemONEncoder where
    encode (Message pairs _ _) = encodePairs pairs

encodePair :: SPair a -> String
encodePair (SPair ident t _) = encodeIdentifier ident ++ ": " ++ encodeType t

encodePairs :: [SPair a] -> String
encodePairs [] = ""
encodePairs [x] = encodePair x
encodePairs (x:xs) = encodePair x ++ ", " ++ encodePairs xs

encodeType :: SType a -> String
encodeType t = case t of
    TStr _ -> "str"
    TInt _ -> "int"
    TFloat _ -> "float"
    TBool _ -> "bool"
    TChar _ -> "char"
    TList a _ -> "[" ++ encodeType a ++ "]"
    TObj a _ -> "{" ++ encodePairs a ++ "}"
    TNullable a _ -> encodeType a ++ "?"
    TCustom ident _ -> encodeIdentifier ident

encodeIdentifier :: Identifier -> String
encodeIdentifier (Identifier s _) = s

----------------------------------- C# Encoder -----------------------------------

data CSharpEncoder = CSharpEncoder
instance Encoder CSharpEncoder where
    encode (Message [] eof t) = ""
    encode (Message (SPair ident innerType _:ps) eof token) = "public class " ++ csEncodeIdentifier ident ++ " " ++ csEncodeClassBody innerType 0 ++ encode (Message ps eof token)

csEncodeIdentifier :: Identifier -> String
csEncodeIdentifier (Identifier (h:s) _) = toUpper h:s

csEncodeClassBody :: SType a -> Int -> String
csEncodeClassBody (TObj inner _) indent = "{\n" ++ csEncodePairs inner (indent + 1) ++ "}\n\n"

csEncodePairs :: [SPair a] -> Int -> String
csEncodePairs [] _ = ""
csEncodePairs (p:ps) indent = csEncodePair p indent ++ "\n" ++ csEncodePairs ps indent

csEncodePair :: SPair a -> Int -> String
csEncodePair (SPair ident t _) indent = csIndent indent ++ "public " ++ csEncodeType t ++ " " ++ csEncodeIdentifier ident ++ " { get; set; }"

csEncodeType :: SType a -> String
csEncodeType (TBool _) = "bool"
csEncodeType (TInt _) = "int"
csEncodeType (TFloat _) = "float"
csEncodeType (TChar _) = "char"
csEncodeType (TStr _) = "string"
csEncodeType (TCustom ident _) = csEncodeIdentifier ident
csEncodeType (TNullable t _) = csEncodeType t ++ "?"
csEncodeType (TList inner _) = "List<" ++ csEncodeType inner ++ ">"
csEncodeType (TObj inner _) = "Dictionary<string, object>"

csIndent :: Int -> String
csIndent n = replicate (n * 4) ' '

