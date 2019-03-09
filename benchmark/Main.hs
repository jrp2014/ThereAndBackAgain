module Main
  ( main
  ) where

import Gauge.Main
import ThereAndBackAgain

xs :: [Int]
xs = [1 .. 10000]

ys :: [Int]
ys = [10001 .. 20000]

lxs :: Int
lxs = length xs

main :: IO ()
main =
  defaultMain
    [ bench "convolutionT" (nf (convolutionT xs) ys)
    , bench "convolveTABA" (nf (convolveTABA xs) ys)
    , bench "convolutionC" (nf (convolutionC xs) ys)
    , bench "convolutionD" (nf (convolutionD xs) ys)
    , bench "cnv1" (nf cnv1 (xs, ys))
    , bench "cnv2" (nf cnv2 (xs, ys))
    , bench "cnv3" (nf cnv3 (xs, ys))
    , bench "cnv3'" (nf (cnv3' xs) ys)
    , bench "reverse" (nf reverse xs)
    , bench "tabaRev" (nf tabaRev xs)
    , bench "tabaRevOpt" (nf tabaRevOpt xs)
--    , bench "catalan" (whnf catalan (10 :: Int))
    , bench "cnvHalves" (nf cnvHalves (xs, lxs))
    ]
