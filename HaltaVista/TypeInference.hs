module HaltaVista.TypeInference (infer) where

import HaltaVista.Types
import Language.Haskell.Interpreter hiding (infer)
import Data.List
import Control.Exception(throw)

convert (Left e)         = Left (show e) --return Nothing
convert (Right (Just x)) = Right x
convert (Right Nothing)  = Left "does not typecheck"

load = do setImports ["Prelude"]
          set [installedModulesInScope := True]

infer :: [([Input],Output)] -> IO (Either String Type)
infer ios = runInterpreter (load >> infer' ios) >>= return . convert

infer' ios = do ok <- typeChecks fun
                if ok then Just `fmap` typeOf fun
                      else return Nothing
             where fun = synth ios

synth :: [([Input],Output)] -> String
synth ios = "let " ++ glue (map clause ios) ++ " in foo"
  where glue xs = "{" ++ intercalate ";" xs ++ "}"
        clause (is,o) = "foo " ++ unwords (map parens is)
                               ++ " = " ++ o
