import HaltaVista.Types
import HaltaVista.TypeInference
import HaltaVista.Hoogle
import HaltaVista.Match

import Control.Monad(filterM, when)

suffix = reverse . takeWhile (/= '.') . reverse

search :: [([Input], Output)] -> IO [Answer]
search ios = do
  mty <- infer ios
  case mty of
    Left e   -> error (show e)
    Right ty -> do
      --putStrLn ("ty = " ++ (suffix ty))
      hoogle (suffix ty)

split xs = split' [] xs
  where split' acc [x]    = (reverse acc,x)
        split' acc (x:xs) = split' (x:acc) xs

parse :: String -> [([Input], Output)]
parse = map split . map words . lines 

pretty :: Answer -> String
pretty (x,y) = x ++ " " ++ y

main = do
  ios <- parse `fmap` getContents
  candidates <- search ios
  let pp cand = do ok <- matches cand ios
                   when ok $ putStrLn $ pretty cand
  mapM_ pp candidates
