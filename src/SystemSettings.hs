module SystemSettings(evalPath,
                      compileString,
                      runString,
                      dataFileName,
                      cleanupCommand,
                      executableName) where

import Data.List as L

evalPath = "/Users/dillon/Haskell/Mini/eval_folder/"

cleanupCommand filePath = "rm -rf " ++ (dataFileName filePath)
compileString filePath = "clang -O3 -mavx -march=native -mfma -o " ++ (executableName filePath) ++ " " ++ filePath
runString filePath = executableName filePath
dataFileName filePath = (L.takeWhile (\c -> c /= '.') filePath) ++ ".txt"
executableName filePath = L.takeWhile (\c -> c /= '.') filePath
