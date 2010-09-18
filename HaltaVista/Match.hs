module HaltaVista.Match (matches) where

import HaltaVista.Types
import qualified Language.Haskell.Interpreter as I
import Control.Monad((<=<))
import Data.List
import System.Timeout(timeout)
import Control.Exception(handle, SomeException)
import Debug.Trace(trace)
import System.IO(hFlush, stdout)
import System.IO.Unsafe
import Prelude hiding (catch)
import Control.Concurrent

thres = 3000000

matches :: (String,String) -> [([Input], Output)] -> IO Bool
matches (m,f) ios = do putStrLn ("mod, fun = " ++ show (m,f))
                       hFlush stdout
                       x <- handle guard $
                            coerce_just `fmap` timeout thres command
                       putStrLn ("res = " ++ show x)
                       hFlush stdout
                       return x
    where command :: IO Bool
          command = do res <- I.runInterpreter $ do I.setImports ["Prelude",m]
                                                    check f ios
                       -- runInterpreter returns Right x if the
                       -- evaluation throws an exception.  To ensure
                       -- this x is evaluated (and the exception
                       -- thrown) within `command', we need to seq it
                       -- here.
                       case res of
                         Right x -> x `seq` return x
                         _ -> return False
          guard :: SomeException -> IO Bool
          guard _ = return False
          coerce_just (Just b) = b
          coerce_just Nothing  = False

input f = prefix . intercalate " " . map I.parens
  where prefix x = f ++ " " ++ x

run f ios = mapM (I.eval . input f) ios

check f ios = (and . zipWith (==) os) `fmap` run f is
  where (is,os) = unzip ios



