module SanityCheckReport() where

import Data.List as L

import IndexExpression
import IndexExpressionOptimizations
import MiniOperation
import MOpSyntax
import RuntimeEvaluation
import SymbolTable

timeOps = timeOperationsWithOptimizations "" "multi_op_sc" (L.map convertToMini operations) optimizations

operations =
  [mOp "one_matrix_subtract_RS_3_17" (maddOpSymRS 3 17) [msub "a" "b" "c"],
   mOp "one_matrix_subtract_RS_9_2" (maddOpSymRS 9 2) [msub "a" "b" "c"],
   mOp "one_matrix_subtract_CS_1_1" (maddOpSymCS 1 1) [msub "a" "b" "c"],
   mOp "one_matrix_subtract_CS_15_123" (maddOpSymCS 15 123) [msub "a" "b" "c"]]

argLayoutRS nr nc = layout (iConst nr) (iConst nc) (iConst nc) (iConst 1)
argLayoutCS nr nc = layout (iConst nr) (iConst nc) (iConst 1) (iConst nr)

maddOpSymRS nr nc = mOpSymtab [("a", mOpSymInfo arg doubleFloat $ argLayoutRS nr nc),
                               ("b", mOpSymInfo arg doubleFloat $ argLayoutRS nr nc),
                               ("c", mOpSymInfo arg doubleFloat $ argLayoutRS nr nc)]

maddOpSymCS nr nc = mOpSymtab [("a", mOpSymInfo arg doubleFloat $ argLayoutCS nr nc),
                               ("b", mOpSymInfo arg doubleFloat $ argLayoutCS nr nc),
                               ("c", mOpSymInfo arg doubleFloat $ argLayoutCS nr nc)]

optimizations =
  [evalIExprConstants]
