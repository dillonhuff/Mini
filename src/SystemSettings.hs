module SystemSettings(compileString,
                      runString,
                      dataFileName,
                      cFileName,
                      executableFileName) where

import Data.List as L

projectPath = "/Users/dillon/Haskell/Mini/"
dataFilePath = projectPath ++ "run_data/"
evalPath = projectPath ++ "eval_folder/"

dataFileName opName = dataFilePath ++ opName ++ ".txt"
cFileName opName = evalPath ++ opName ++ ".c"
executableFileName opName = evalPath ++ opName

compileString opName = "clang -O3 -mavx -march=native -mfma -o " ++ (executableFileName opName) ++ " " ++ (cFileName opName)
runString opName = executableFileName opName
