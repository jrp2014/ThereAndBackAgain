module ThereAndBackAgain where

-- https://www.brics.dk/RS/01/39/BRICS-RS-01-39.pdf

convolutionT :: [a] -> [b] -> [(a, b)]
convolutionT xs ys = zipT xs (reverse ys)
  where
    zipT [] [] = []
    zipT (a:as) (b:bs) = (a, b) : zipT as bs

convolutionC :: [a] -> [b] -> [(a, b)]
convolutionC xs ys = walk xs fst
  where
    walk [] k = k ([], ys)
    walk (a:as) k = walk as (\(zs, b:bs) -> k ((a, b) : zs, bs))

convolutionD xs ys = fst . walk $ xs
  where
    walk [] = ([], ys)
    walk (x:xs) =
      let (zs, y:ys) = walk xs
       in ((x, y) : zs, ys)
