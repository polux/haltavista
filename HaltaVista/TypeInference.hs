module HaltaVista.TypeInference (infer) where

import HaltaVista.Types
import Language.Haskell.Interpreter hiding (infer)
import Data.List

convert (Left _)  = Left "wrong input"
convert (Right x) = Right (unwords $ lines x)

load = do setImports ["Prelude"]
          set [installedModulesInScope := True]

infer :: [([Input],Output)] -> IO (Either String Type)
infer ios = runInterpreter (load >> typeOf (synth ios)) >>= return . convert

synth :: [([Input],Output)] -> String
synth ios = prelude ++ "let " ++ body (map clause ios) ++ " in foo"
  where body xs = "{" ++ intercalate ";" xs ++ "}"
        args = map (("haltavista_x"++).show) [1..length (fst.head $ ios)]
        lhs = "foo " ++ unwords args
        clause (is,o) = lhs ++ " = let " ++ body (zipWith constr args is) ++ " in " ++ parens o
        constr a i = "() = equate " ++ a ++ " " ++ parens i

prelude = "let equate = (undefined :: a -> a -> ()) in "
