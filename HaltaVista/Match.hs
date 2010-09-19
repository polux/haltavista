module HaltaVista.Match (matches) where

import HaltaVista.Types
import qualified Language.Haskell.Interpreter as I
import Control.Monad((<=<))
import Data.List
import System.Timeout(timeout)
import Control.Exception(handle, SomeException)

thres = 1500000

matches :: (String,String) -> [([Input], Output)] -> IO Bool
matches (m,f) ios = do x <- handle guard $
                            coerceJust `fmap` timeout thres command
                       return x
    where command :: IO Bool
          command = do res <- I.runInterpreter $ do I.setImports ["Prelude", m]
                                                    check f ios
                       {- runInterpreter returns Right x if the evaluation
                        - throws an exception.  To ensure this x is evaluated
                        - (and the exception thrown) within `command', we need
                        - to seq it here. -}
                       case res of
                         Right x -> x `seq` return x
                         _       -> return False
          guard :: SomeException -> IO Bool
          guard _ = return False
          coerceJust (Just b) = b
          coerceJust Nothing  = False

input f = prefix . intercalate " " . map I.parens
  where prefix x = f ++ " " ++ x

run f ios = mapM (I.eval . input f) ios

check f ios = (and . zipWith (==) os) `fmap` run f is
  where (is,os) = unzip ios



