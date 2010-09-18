module HaltaVista.Match (matches) where

import HaltaVista.Types
import qualified Language.Haskell.Interpreter as I
import Control.Monad((<=<))
import Data.List
import System.Timeout(timeout)
import Control.Exception(try, SomeException)
import Debug.Trace(trace)

thres = 3000000

matches :: (String,String) -> [([Input], Output)] -> IO Bool
matches (m,f) ios = --putStrLn ("mod, fun = " ++ show (m,f)) >> 
                    convert `fmap` handle res
                    -- >>= (\x -> putStrLn ("res = " ++ show x) >> return x)
  where res = I.runInterpreter $ I.setImports ["Prelude",m] >> check f ios
        handle :: IO a -> IO (Maybe (Either SomeException a))
        handle io = timeout thres (try io)
        convert (Just (Right (Right x))) = x
        convert y                        = False


input f = prefix . intercalate " " . map I.parens
  where prefix x = f ++ " " ++ x

run f ios = mapM (I.eval . input f) ios

check f ios = (and . zipWith (==) os) `fmap` run f is
  where (is,os) = unzip ios



