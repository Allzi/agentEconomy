module SimUtils where
import Data.List
import Data.Function
import Control.Monad.State

--------------Simulation with random-------------------

class Monad a => RSim a where
    setRandom :: [Double] -> a ()
    getRandom :: a [Double]

randSim :: RSim s => ([Double] -> ([Double], a)) -> s a
randSim randFunc = do
    rs <- getRandom
    let (rs', a) = randFunc rs
    setRandom rs'
    return a

randIds :: Eq a => [Double] -> [a] -> Int -> Int-> ([Double],[a])
randIds rs _ _ 0 = (rs, [])
randIds rs [] _ _ = (rs, [])
randIds (r:rs) ids idNum toTake = (rs', i:ls)
  where 
    (rs', ls) = randIds rs ids' (idNum-1) (toTake-1)
    rind = floor $ r*fromIntegral idNum
    i = ids !! rind
    ids' = delete i ids

shuffle :: (RSim s, Eq a) => [a]-> Int -> s [a]
shuffle l n =
    randSim (\rs -> randIds rs l n n)


----------------------------Market-----------------------------
-- A simple s&m market design from Riccetti 2012

data Seller = Seller {
    sellerId :: Int,
    ask ::  Double
    } deriving Eq

data Buyer = Buyer {
    buyerId :: Int,
    bid :: Double
    } deriving Eq

type Matcher s = Seller -> Buyer -> s (Seller, Buyer)

marketLoop :: RSim s => Int -> Matcher s -> [Seller] -> [Buyer] -> s ()
marketLoop trials f sups dems = do
    -- random order
    shuffled <- shuffle dems (length dems)
    -- round of matching
    (sups', dems') <- foldM (tryMatch trials f) (sups, []) shuffled
    -- check if there are possible matches left
    unless (shouldStop sups' dems') $
        marketLoop trials f sups' dems'
  where
    shouldStop ss ds = if (null ss) || (null ds)
        then True 
        else if (smallAsk ss) > (bigBid ds)
            then True
            else False
    smallAsk ss = ask $ minimumBy (compare `on` ask) ss
    bigBid ds   = bid $ maximumBy (compare `on` bid) ds


tryMatch :: RSim s => Int -> Matcher s -> ([Seller], [Buyer]) -> 
            Buyer -> s ([Seller], [Buyer])
tryMatch trials f (sups, ds) d = do
    -- take random suppliers
    rSups <- randSim $ (\rs -> randIds rs sups (length sups) trials)
    -- choose cheapest offer and match
    let best = minimumBy (compare `on` ask) rSups
    (s', d') <- f best d
    -- filter out exhausted demand/supply (no ask or pid)
    let newSups = if (ask s' == 0)
            then deleteBy ((==) `on` sellerId) s' sups
            else s':(deleteBy ((==) `on` sellerId) s' sups)
        newDs = if (bid d' == 0)
            then ds
            else d':ds
    return (newSups, newDs)
