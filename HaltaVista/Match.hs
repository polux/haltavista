module HaltaVista.Match (matches) where

import HaltaVista.Types
import qualified Language.Haskell.Interpreter as I
import Control.Monad((<=<))
import Data.List

convert (Left _)  = return False
convert (Right x) = return x

--load = I.setImports ["Prelude", "Data.List"]

matches :: (String,String) -> [([Input], Output)] -> IO Bool
matches (m,f) ios = convert =<< I.runInterpreter (I.setImports [m] >> check f ios)

input f = prefix . intercalate " " . map parens
  where parens x = "(" ++ x ++ ")"
        prefix x = f ++ " " ++ x

run f ios = mapM (I.eval . input f) ios

check f ios = (and . zipWith (==) os) `fmap` run f is
  where (is,os) = unzip ios
