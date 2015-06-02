module FrontEnd.Token(Token,
             pos,
             res, ident, lit, intLit,
             dres, dident, dlit, dIntLit,
             isIdent, isRes, isIntLit,
             resName, identName, litInt,
             dummyPos) where

import Text.Parsec.Pos

data Token
  = Identifier String SourcePos
  | ResWord String SourcePos
  | Literal Lit SourcePos
    deriving (Ord, Show)

pos (Identifier _ p) = p
pos (ResWord _ p) = p
pos (Literal _ p) = p

ident s p = Identifier s p
res s p = ResWord s p
lit l p = Literal l p
intLit n p = Literal (ILit n) p

dident s = Identifier s dummyPos
dres s = ResWord s dummyPos
dlit l = Literal l dummyPos
dIntLit n = Literal (ILit n) dummyPos

isIdent (Identifier _ _) = True
isIdent _ = False

isRes (ResWord _ _) = True
isRes _ = False

isIntLit (Literal (ILit _) _) = True
isIntLit _ = False

litInt (Literal (ILit n) _) = n

resName (ResWord n _) = n
identName (Identifier n _) = n

dummyPos = newPos "DUMMY" 0 0

instance Eq Token where
  (==) (Identifier n1 _) (Identifier n2 _) = n1 == n2
  (==) (ResWord n1 _) (ResWord n2 _) = n1 == n2
  (==) (Literal l1 _) (Literal l2 _) = l1 == l2
  (==) _ _ = False

data Lit
  = DLit Double
  | ILit Int
    deriving (Eq, Ord, Show)
