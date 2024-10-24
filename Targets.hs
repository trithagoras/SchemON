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

encodeTypes :: [SType a] -> String
encodeTypes [] = ""
encodeTypes [x] = encodeType x
encodeTypes (x:xs) = encodeType x ++ ", " ++ encodeTypes xs

encodeType :: SType a -> String
encodeType t = case t of
    TStr _ -> "str"
    TInt _ -> "int"
    TFloat _ -> "float"
    TBool _ -> "bool"
    TChar _ -> "char"
    TTuple a _ -> "(" ++ encodeTypes a ++ ")"
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

csEncodeTypes :: [SType a] -> String
csEncodeTypes [] = ""
csEncodeTypes [x] = csEncodeType x
csEncodeTypes (x:xs) = csEncodeType x ++ ", " ++ csEncodeTypes xs

csEncodeType :: SType a -> String
csEncodeType (TBool _) = "bool"
csEncodeType (TInt _) = "int"
csEncodeType (TFloat _) = "float"
csEncodeType (TChar _) = "char"
csEncodeType (TStr _) = "string"
csEncodeType (TCustom ident _) = csEncodeIdentifier ident
csEncodeType (TNullable t _) = csEncodeType t ++ "?"
csEncodeType (TList inner _) = "List<" ++ csEncodeType inner ++ ">"
csEncodeType (TTuple inner _) = "(" ++ csEncodeTypes inner ++ ")"
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

tsEncodeTypes :: [SType a] -> Int -> String
tsEncodeTypes [] indent = ""
tsEncodeTypes [x] indent = tsEncodeType x indent
tsEncodeTypes (x:xs) indent = tsEncodeType x indent ++ ", " ++ tsEncodeTypes xs indent

tsEncodeType :: SType a -> Int -> String
tsEncodeType (TBool _) _ = "boolean"
tsEncodeType (TInt _) _ = "number"
tsEncodeType (TFloat _) _ = "number"
tsEncodeType (TChar _) _ = "string"
tsEncodeType (TStr _) _ = "string"
tsEncodeType (TCustom ident _) _ = tsEncodeIdentifierUpper ident
tsEncodeType (TNullable t _) ind = tsEncodeType t ind ++ " | undefined"
tsEncodeType (TList inner _) ind = tsEncodeType inner ind ++ "[]"
tsEncodeType (TTuple inner _) ind = "[" ++ tsEncodeTypes inner ind ++ "]"
tsEncodeType (TObj inner t) ind = tsEncodeClassBody (TObj inner t) ind

-------------------------------------- Go Encoder --------------------------------------

data GoEncoder = GoEncoder
encode' (Message (SPair ident innerType _:ps) eof token) = "type " ++ goEncodeIdentifier ident ++ " struct " ++ goEncodeStructBody innerType 0 ++ encode (Message ps eof token)
instance Encoder GoEncoder where
    encode (Message [] eof t) = ""
    encode (Message ps eof token) = "package schemon\n\n" ++ encode' (Message ps eof token)

goEncodeIdentifier :: Identifier -> String
goEncodeIdentifier (Identifier (h:s) _) = toUpper h:s

goEncodeStructBody :: SType a -> Int -> String
goEncodeStructBody (TObj inner _) ind = "{\n" ++ goEncodePairs inner (ind + 1) ++ indent ind ++ "}"

goEncodePairs :: [SPair a] -> Int -> String
goEncodePairs [] _ = ""
goEncodePairs [p] ind = goEncodePair p ind ++ "\n"
goEncodePairs (p:ps) ind = goEncodePair p ind ++ "\n" ++ goEncodePairs ps ind

goEncodePair :: SPair a -> Int -> String
goEncodePair (SPair ident t _) ind = indent ind ++ goEncodeIdentifier ident ++ " " ++ goEncodeType t ind

goEncodeTupleElements :: [SType a] -> Int -> String
goEncodeTupleElements [] _ = ""
goEncodeTupleElements [x] ind = goEncodeTupleElement x ind 1
goEncodeTupleElements (x:xs) ind = goEncodeTupleElement x ind 1 ++ goEncodeTupleElements' xs ind 2
  where
    goEncodeTupleElements' [] _ _ = ""
    goEncodeTupleElements' (x:xs) ind no = goEncodeTupleElement x ind no ++ goEncodeTupleElements' xs ind (no + 1) ++ indent ind


goEncodeTupleElement :: SType a -> Int -> Int -> String
goEncodeTupleElement x ind no = indent (ind + 1) ++ "Item" ++ show no ++ " " ++ goEncodeType x ind ++ "\n"

goEncodeTypes :: [SType a] -> Int -> String
goEncodeTypes [] indent = ""
goEncodeTypes [x] indent = goEncodeType x indent
goEncodeTypes (x:xs) indent = goEncodeType x indent ++ ", " ++ goEncodeTypes xs indent

goEncodeType :: SType a -> Int -> String
goEncodeType (TBool _) _ = "bool"
goEncodeType (TInt _) _ = "int"
goEncodeType (TFloat _) _ = "float64"
goEncodeType (TChar _) _ = "rune"
goEncodeType (TStr _) _ = "string"
goEncodeType (TCustom ident _) _ = "*" ++ goEncodeIdentifier ident
goEncodeType (TNullable t _) ind = goEncodeType t ind
goEncodeType (TList inner _) ind = "[]" ++ goEncodeType inner ind
goEncodeType (TTuple inner _) ind = "*struct {\n" ++ goEncodeTupleElements inner ind ++ "}"
goEncodeType (TObj inner t) ind = "*struct " ++ goEncodeStructBody (TObj inner t) ind

----------------------------------- Shared functions -----------------------------------

indent :: Int -> String
indent n = replicate (n * 4) ' '

