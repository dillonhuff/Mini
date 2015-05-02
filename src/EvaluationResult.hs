module EvaluationResult(EvaluationResult,
                        evaluationResult,
                        passedSanityCheck) where


data EvaluationResult
  = EvaluationResult [Int] Bool
    deriving (Eq, Ord, Show)

evaluationResult = EvaluationResult

passedSanityCheck (EvaluationResult _ b) = b

