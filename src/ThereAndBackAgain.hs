module ThereAndBackAgain where

-- https://www.brics.dk/RS/01/39/BRICS-RS-01-39.pdf
-- 2002
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

convolutionD :: [a] -> [b] -> [(a, b)]
convolutionD xs ys = fst . walk $ xs
  where
    walk [] = ([], ys)
    walk (x:xs) =
      let (zs, y:ys) = walk xs
       in ((x, y) : zs, ys)

-- 2005
cnv1 :: ([a], [b]) -> [(a, b)]
cnv1 (xs, ys) = walk (xs, [])
  where
    walk ([], a) = continue (a, ys, [])
    walk (x:xs, a) = walk (xs, x : a)
    continue ([], [], r) = r
    continue (x:a, y:ys, r) = continue (a, ys, (x, y) : r)

cnv2 :: ([a], [b]) -> [(a, b)]
cnv2 (xs, ys) = walk (xs, \([], r) -> r)
  where
    walk ([], k) = k (ys, [])
    walk (x:xs, k) = walk (xs, \(y:ys, r) -> k (ys, (x, y) : r))

cnv3 :: ([a], [b]) -> [(a, b)]
cnv3 (xs, ys) = snd . walk $ xs
  where
    walk [] = (ys, [])
    walk (x:xs) =
      let (y:ys, r) = walk xs
       in (ys, (x, y) : r)

tabaRev :: [a] -> [a]
tabaRev xs = snd . walk $ xs
  where
    walk [] = (xs, [])
    walk (_:xs) =
      let (y:ys, r) = walk xs
       in (ys, y : r)

tabaRevOpt :: [a] -> [a]
tabaRevOpt xs = walkReturn (xs, [])
  where
    walkReturn ([], r) = r
    walkReturn (y:ys, r) = walkReturn (ys, y : r)

suffixes :: ([a], [b]) -> [[(a, b)]]
suffixes ([], []) = []
suffixes xy@(_:xs, _:ys) = cnv3 xy : suffixes (xs, ys)

prefixes :: ([a], [b]) -> [[(a, b)]]
prefixes (xs, ys) = walk (xs, snd)
  where
    walk ([], k) = [k (ys, [])]
    walk (x:xs, k) = k (ys, []) : walk (xs, \(y:ys, r) -> k (ys, (x, y) : r))

catalan :: (Ord a, Num a) => a -> a
catalan m = iter (1, [1])
  where
    cat a = snd . walk $ a
      where
        walk [] = (a, 0)
        walk (n:ns) =
          let (n':ns', r) = walk ns
           in (ns', r + (n * n'))
    iter (i, a@(b:bs))
      | i > m = b
      | otherwise = iter (i + 1, cat a : a)

cnvHalves (xs, n) = snd . walk $ (n, xs)
  where
    walk (0, xs) = (xs, [])
    walk (n, x:xs) =
      let (y:ys, r) = walk (n - 2, xs)
       in (ys, (x, y) : r)
