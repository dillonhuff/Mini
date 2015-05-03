module Token(Token,
             res, ident, lit,
             dres, dident, dlit) where

import Text.Parsec.Pos

data Token
  = Identifier String SourcePos
  | ResWord String SourcePos
  | Literal Lit SourcePos
    deriving (Ord, Show)

ident s p = Identifier s p
res s p = ResWord s p
lit l p = Literal l p

dident s = Identifier s dummyPos
dres s = ResWord s dummyPos
dlit l = Literal l dummyPos

dummyPos = newPos "DUMMY" 0 0

instance Eq Token where
  (==) (Identifier n1 _) (Identifier n2 _) = n1 == n2
  (==) (ResWord n1 _) (ResWord n2 _) = n1 == n2
  (==) (Literal l1 _) (Literal l2 _) = l1 == l2

data Lit
  = DLit Double
  | ILit Int
    deriving (Eq, Ord, Show)
