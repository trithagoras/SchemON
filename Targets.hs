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
csEncodeClassBody (TObj inner _) ind = "{\n" ++ csEncodePairs inner (ind + 1) ++ "}\n\n"

csEncodePairs :: [SPair a] -> Int -> String
csEncodePairs [] _ = ""
csEncodePairs (p:ps) ind = csEncodePair p ind ++ "\n" ++ csEncodePairs ps ind

csEncodePair :: SPair a -> Int -> String
csEncodePair (SPair ident t _) ind = indent ind ++ "public " ++ csEncodeType t ++ " " ++ csEncodeIdentifier ident ++ " { get; set; }"

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

----------------------------------- TypeScript Encoder -----------------------------------

data TypeScriptEncoder = TypeScriptEncoder
instance Encoder TypeScriptEncoder where
    encode (Message [] eof t) = ""
    encode (Message (SPair ident innerType _:ps) eof token) = "interface " ++ tsEncodeIdentifierUpper ident ++ " " ++ tsEncodeClassBody innerType 0 ++ "\n\n" ++ encode (Message ps eof token)

tsEncodeIdentifier :: Identifier -> String
tsEncodeIdentifier (Identifier s _) = s

tsEncodeIdentifierUpper :: Identifier -> String
tsEncodeIdentifierUpper (Identifier (h:s) _) = toUpper h:s

tsEncodeClassBody :: SType a -> Int -> String
tsEncodeClassBody (TObj inner _) ind = "{\n" ++ tsEncodePairs inner (ind + 1) ++ indent ind ++ "}"

tsEncodePairs :: [SPair a] -> Int -> String
tsEncodePairs [] _ = ""
tsEncodePairs [p] ind = tsEncodePair p ind ++ "\n"
tsEncodePairs (p:ps) ind = tsEncodePair p ind ++ ",\n" ++ tsEncodePairs ps ind

tsEncodePair :: SPair a -> Int -> String
tsEncodePair (SPair ident t _) ind = indent ind ++ tsEncodeIdentifier ident ++ ": " ++ tsEncodeType t ind

tsEncodeType :: SType a -> Int -> String
tsEncodeType (TBool _) _ = "boolean"
tsEncodeType (TInt _) _ = "number"
tsEncodeType (TFloat _) _ = "number"
tsEncodeType (TChar _) _ = "string"
tsEncodeType (TStr _) _ = "string"
tsEncodeType (TCustom ident _) _ = tsEncodeIdentifierUpper ident
tsEncodeType (TNullable t _) ind = tsEncodeType t ind ++ " | undefined"
tsEncodeType (TList inner _) ind = tsEncodeType inner ind ++ "[]"
tsEncodeType (TObj inner t) ind = tsEncodeClassBody (TObj inner t) ind

----------------------------------- Shared functions -----------------------------------

indent :: Int -> String
indent n = replicate (n * 4) ' '

