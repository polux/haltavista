module HaltaVista.TypeInference (infer) where

import HaltaVista.Types
import qualified Language.Haskell.Interpreter as I
import Data.List

convert (Left _)  = return Nothing
convert (Right x) = return x

load = I.setImports ["Prelude", "Data.List"]

infer :: [([Input],Output)] -> IO (Maybe Type)
infer ios = I.runInterpreter (load >> infer' ios) >>= convert

infer' ios = do ok <- I.typeChecks fun
                if ok then Just `fmap` I.typeOf fun
                      else return Nothing
             where fun = synth ios

synth :: [([Input],Output)] -> String
synth ios = "let " ++ glue (map clause ios) ++ " in foo"
  where parens x = "(" ++ x ++ ")"
        glue xs = "{" ++ intercalate ";" xs ++ "}"
        clause (is,o) = "foo " ++ unwords (map parens is)
                               ++ " = " ++ o
