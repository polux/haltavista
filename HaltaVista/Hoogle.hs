-- | copy-pasted from Yi.Hoogle and modified

module HaltaVista.Hoogle (hoogle) where

import System.Exit (ExitCode(ExitFailure))
import System.Process
import Data.List(isInfixOf)
import Control.Monad(when)
import Control.Arrow((&&&))
import Control.Applicative((<$>))

-- | Hoogle's output includes a sort of type keyword, telling whether a hit is a package name, syntax,
-- a module name, etc. But we care primarily about the function names, so we filter out anything containing
-- the keywords.
gv :: [String] -> [String]
gv = filter f
  where f x = not $ any (`isInfixOf` x) ["module ", " type ", "package ", " data ", " keyword "]

-- | Query Hoogle, with given search and options. This errors out on no
-- results or if the hoogle command is not on path.
hoogleRaw :: String -> String -> IO [String]
hoogleRaw srch opts = do (status,out,err) <- readProcessWithExitCode "hoogle" [opts, srch] ""
                         when (status == ExitFailure 1) $ fail err 
                         let results = lines out
                         if results == ["No results found"] then fail "No Hoogle results"
                                                            else return results

-- | Return module-function pairs.
hoogle :: String -> IO [(String, String)]
hoogle a = map ((head &&& (!! 1)) . words) . gv  <$> hoogleRaw a ""
