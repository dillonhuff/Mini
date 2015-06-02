module Testing.EvaluationResult(EvaluationResult,
                        evaluationResult,
                        avgCyclesPerRun,
                        passedSanityCheck) where


data EvaluationResult
  = EvaluationResult Double Bool
    deriving (Eq, Ord, Show)

evaluationResult a b = EvaluationResult a b

passedSanityCheck (EvaluationResult _ b) = b
avgCyclesPerRun (EvaluationResult a _) = a
