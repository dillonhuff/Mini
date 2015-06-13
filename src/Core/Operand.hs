module Core.Operand(Operand,
                    reg, bufferVal,
                    isBufferVal, bufferName,
                    registerName,
                    operandName,
                    operandsHaveSameType,
                    accessIExpr) where

import Core.IndexExpression

data Operand
  = Register String
  | BufferVal String IExpr
    deriving (Eq, Ord, Show)

reg s = Register s
bufferVal s i = BufferVal s i

isBufferVal (BufferVal _ _) = True
isBufferVal _ = False

operandsHaveSameType (Register _) (Register _) = True

operandName op =
  case isBufferVal op of
    True -> bufferName op
    False -> registerName op

bufferName (BufferVal s _) = s

accessIExpr (BufferVal _ i) = i

registerName (Register s) = s
registerName other = error $ "cannot get register name for buffer " ++ show other
