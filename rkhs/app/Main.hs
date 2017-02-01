module Main where

import Lib

main :: IO ()
main = putStrLn . show $ 1 / einv
  where
    xs = integrate 0 [1] $ rk4 0.0001 (\(t, [x]) -> [-x])
    (_, [einv]) = xs !! 10000
