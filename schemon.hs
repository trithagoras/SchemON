-- SchemON (Schema Object Notation)
data SON = SON String | Err [String] deriving (Show)

data SType a = TStr | TInt | TFloat | TBool | TChar | TList (SType a) | TObj [SPair a] deriving (Show)
data SPair a = SPair String (SType a) deriving (Show)

son :: SON
root :: SType a

-- the following are equivalent and a result of parsing
son = SON "{name: str, age: int, phoneBook: [{name: str, number: str}]}"
root = TObj [SPair "name" TStr, SPair "age" TInt, SPair "phonebook" (TList (TObj [SPair "name" TStr, SPair "number" TStr]))]

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

test = encodeType root