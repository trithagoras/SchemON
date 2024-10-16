class Encoder a where
    encode :: Program a -> String

data SchemONEncoder = SchemONEncoder
instance Encoder SchemONEncoder where
    encode (Root t) = encodeType t

newtype Program a = Root (SType a) deriving (Show)

data SType a = TStr | TInt | TFloat | TBool | TChar | TList (SType a) | TObj (ObjRValue a) | TDateTime | String deriving (Show)
data SPair a = SPair String (SType a) deriving (Show)

type ObjRValue a = [SPair a]
newtype Message a = Message (ObjRValue a)

program :: Program a
person :: SType a

program = Root person
person = TObj [SPair "name" TStr, SPair "age" TInt, SPair "phonebook" (TList (TObj [SPair "name" TStr, SPair "number" TStr]))]

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
    TDateTime -> "datetime"
    TList a -> "[" ++ encodeType a ++ "]"
    TObj a -> "{" ++ encodePairs a ++ "}"

test = encode (program :: Program SchemONEncoder)
