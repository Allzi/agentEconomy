{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, ViewPatterns #-}
module SimUtils where
import Control.Applicative
import Data.Random
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST
import qualified Data.Map as M

-- * Simulation handling

-- | Type for storing output of simulation.
type SimData = M.Map String SimTable
type SimTable = M.Map String [Double]

dataToTable :: String -> [(String, Double)] -> SimData -> SimData
dataToTable key ls sd = M.insertWith mergeAdd key ms sd
  where
    mergeAdd = M.unionWith (++)
    ms = fmap (\a -> [a]) $ M.fromList ls


addSDPoint :: (String, Double) -> SimTable -> SimTable
addSDPoint (key, d) = M.insertWith (++) key [d]

addSDPoints :: [(String, Double)] -> SimTable -> SimTable
addSDPoints [] = id
addSDPoints (dp:sdat) = (addSDPoint dp).(addSDPoints sdat)


-- | MapMSim traverses trough a traversable in (Simulation) State, executing
-- the result and modifying it.
-- <%=> works just like %=, but gets function in the State monad instead.
-- The action should not directly depend on other members of the traversed 
-- data structure.
mapMSim, (<%=>) :: (Traversable t, Monad (StateT s m), Monad m, Functor m) =>
    (Lens' s (t a)) -> (a -> StateT s m a) -> StateT s m ()
{-# INLINE mapMSim #-}
mapMSim l sim = (l .=) =<< (use l <&> traverse sim & join)


infixl 4 <%=> 
(<%=>) = mapMSim

-- * Random Utilities

-- | Get a random element from a vector.
-- O(1)
randomElementV :: V.Unbox a =>  V.Vector a -> RVar a
{-# INLINE randomElementV #-}
randomElementV vec
  | V.null vec = error "empty vector"
  | otherwise = do
    let l = V.length vec
    i <- uniformT 0 (l - 1)
    return (V.unsafeIndex vec i)

-- | Shuffle for all those id-vectors!
-- For performance!
shuffleV :: V.Unbox a => V.Vector a -> RVar (V.Vector a)
{-# INLINE shuffleV #-}
shuffleV vec
  | V.null vec = return vec
  | otherwise  = do
    let lst = V.length vec - 1
    ind <- V.generateM lst (\i -> uniformT 0 (i + 1))
    return $ runST $ do
        mvec <- V.thaw vec
        let loop i
              | i == 0    = V.unsafeFreeze mvec
              | otherwise = do
                  VM.unsafeSwap mvec i (ind `V.unsafeIndex` (i - 1))
                  loop (i - 1)
        loop lst

-- | An utility function, that adjusts uniformly the double given as the second
-- argument. The first argument tells the maximum relative adjustment.
-- Giving negative maximum adjustment makes the adjustment negative.
uniformAdj :: Double -> Double -> RVar Double
{-# INLINE uniformAdj #-}
uniformAdj adj a = (\r -> a * (1 + adj * r)) <$> stdUniform

-- | A random element getter, which saves counting the length of the list.
randomElementN :: Int -> [a] -> RVar a
{-# INLINE randomElementN #-}
randomElementN _ [] = error "Empty list!"
randomElementN n xs = do
    i <- uniformT 0 (n - 1)
    return $ xs !! i

-- | Like randomElementN, but also returns the list without the selected
-- element.
randomElementNR :: Int -> [a] -> RVar (a, [a])
{-# INLINE randomElementNR #-}
randomElementNR _ [] = error "Empty list!"
randomElementNR 0 _  = error "Empty list?!"
randomElementNR n xs = do
    i <- uniformT 0 (n - 1)
    let (xs1, x:xs2) = splitAt i xs
    return (x, xs1 ++ xs2)


-- | Old version of weighted random.
whRandElem' :: [a] -> (a -> Double) -> Double -> RVar a
{-# INLINE whRandElem' #-}
whRandElem' list weight whSum = do
    r <- uniformT 0 whSum
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
{-# INLINE whRandElem #-}
whRandElem ls = do
    r <- stdUniform
    return $ go ls r
  where
    go []        _ = error "Empty list!"
    go ((a, _):[]) _ = a
    go ((a, w):as) r = if w >= r
        then a
        else go as (r-w)



