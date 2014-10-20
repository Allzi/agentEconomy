{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, ViewPatterns #-}
module SimUtils where
import Control.Applicative
import Data.Random

type SimData = [(String, Double)]


--------------------New utilities-------------------------



-- | An utility function, that adjusts uniformly the double given as the second
-- argument. The first argument tells the maximum relative adjustment.
-- Giving negative maximum adjustment makes the adjustment negative.
uniformAdj :: Double -> Double -> RVar Double
uniformAdj adj a = (\r -> a * (1 + adj * r)) <$> stdUniform


randomElementN :: Int -> [a] -> RVar a
randomElementN _ [] = error "Empty list!"
randomElementN n xs = do
    i <- uniformT 0 (n - 1)
    return $ xs !! i

-- | Like randomElementN, but also returns the list without the selected
-- element.
randomElementNR :: Int -> [a] -> RVar (a, [a])
randomElementNR _ [] = error "Empty list!"
randomElementNR 0 _  = error "Empty list?!"
randomElementNR n xs = do
    i <- uniformT 0 (n - 1)
    let (xs1, x:xs2) = splitAt i xs
    return (x, xs1 ++ xs2)


-- | Old version of weighted random.
whRandElem' :: [a] -> (a -> Double) -> Double -> RVar a
whRandElem' list weight m = do
    r <- uniformT 0 m
    fetchElem r list
  where
    fetchElem r (l:ls) = do
        let nr = r - weight l
        if nr <= 0
            then return l
            else fetchElem nr ls
    fetchElem r [] = error ("Empty list or too big sum of weights! " ++  show r)

-- | Gets random element from a list with given weights.
-- New version.
-- Weights are given in the second element of tuple inside the list,
-- and their sum should be one.
whRandElem :: [(a, Double)] -> RVar a
whRandElem ls = do
    r <- stdUniform
    return $ go ls r
  where
    go []        _ = error "Empty list!"
    go ((a, _):[]) _ = a
    go ((a, w):as) r = if w >= r
        then a
        else go as (r-w)



