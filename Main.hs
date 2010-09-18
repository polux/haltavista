import HaltaVista.Types
import HaltaVista.TypeInference
import HaltaVista.Hoogle
import HaltaVista.Match

import Control.Monad(filterM)

search :: [([Input], Output)] -> IO [Answer]
search ios = do
  mty <- infer ios
  case mty of
    Nothing -> return []
    Just ty -> do
      candidates <- take 30 `fmap` hoogle ty
      filterM (flip matches ios) candidates

split xs = split' [] xs
  where split' acc [x]    = (reverse acc,x)
        split' acc (x:xs) = split' (x:acc) xs

parse :: String -> [([Input], Output)]
parse = map split . map words . lines 

pretty :: Answer -> String
pretty (x,y) = x ++ " " ++ y

main = do
  ios <- parse `fmap` getContents
  answers <- search ios
  putStrLn . unlines $ map pretty answers
  

{-
main = do
  ios <- parse `fmap` getContents
  ty <- infer ios
  print ty
  cs <- filterM (flip matches ios) ["(+)","reverse","sort"]
  putStrLn $ unlines cs
-}
  
  

