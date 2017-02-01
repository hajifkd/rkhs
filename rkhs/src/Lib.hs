module Lib
    ( integrate,
      rk4
    ) where

(~*~) :: Num a => a -> [a] -> [a]
c ~*~ v = map (c *) v
infixl 7 ~*~

(~+~) :: Num a => [a] -> [a] -> [a]
xs ~+~ [] = xs
[] ~+~ xs = xs
xs ~+~ ys = zipWith (+) xs ys
infixl 6 ~+~

sumV :: Num a => [[a]] -> [a]
sumV []   = []
sumV xss  = foldr1 (~+~) xss

integrate :: Fractional a => a -> [a] -> ((a, [a]) -> (a, [a])) -> [(a, [a])]
integrate t0 x0s next = res
  where
    res = (t0, x0s):(map next res)

rk4 :: Fractional a => a -> ((a, [a]) -> [a]) -> (a, [a]) -> (a, [a])
rk4 dt dxdt (t, xs) = (t + dt, xnews)
  where
    cs    = [0, 1 / 2, 1 / 2, 1]
    ts    = map ((t +) . (dt *)) cs
    ass   = [[], [1 / 2], [0, 1 / 2], [0, 0, 1]]
    kss   = map dxdt $ zip ts $ map ((xs ~+~) . (dt ~*~) . sumV . zipWith (flip (~*~)) kss) ass
    bs    = [1 / 6, 1 / 3, 1 / 3, 1 / 6]
    xnews = xs ~+~ dt ~*~ (sumV . zipWith (~*~) bs) kss
