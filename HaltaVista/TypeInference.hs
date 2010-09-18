module HaltaVista.TypeInference (infer) where

import HaltaVista.Types
import Language.Haskell.Interpreter hiding (infer)
import Data.List
import Control.Exception(throw)

convert (Left e)  = Left "wrong input"
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
        constr a i = "() = unif " ++ a ++ " " ++ parens i

prelude = "let unif = (undefined :: a -> a -> ()) in "

{-
unif :: a -> a -> ()
unif = undefined

f :: (a -> b) -> (a,c) -> (b,c)
f x y = let { _ = unif x (+1) ; _ = unif y (1,2) } in (2,2)
f x y = let { _ = unif x (+"a") ; _ = unif y ("a",2) } in ("aa",2)
-}
