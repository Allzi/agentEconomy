{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
module SimUtils where
import Data.List
import Data.Function
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.IntMap as M

type SimData = [(String, Double)]

--------------Simulation with random-------------------

class Monad a => RSim a where
    setRandom :: [Double] -> a ()
    getRandom :: a [Double]

get1Rand :: RSim s => s Double
get1Rand = do
    (r:rs) <- getRandom
    setRandom rs
    return r

sampleNormal :: RSim s => Double -> Double -> s Double
sampleNormal a b = do
    r1 <- get1Rand
    r2 <- get1Rand
    let x = (cos (2*pi*r1)) * sqrt (-2*(log r2)) 
    return (a + b*x)

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
randIds [] _ _ _ = error "Limited randoms!"

shuffle :: (RSim s, Eq a) => [a]-> Int -> s [a]
shuffle l n =
    randSim (\rs -> randIds rs l n n)


----------------------------Market-----------------------------
-- A simple s&m market design from Riccetti 2012

data Seller = Seller {
    sellerId :: Int,
    askPrice ::  Double
    } deriving Eq

data Buyer = Buyer {
    buyerId :: Int,
    bidPrice :: Double
    } deriving Eq

type Matcher s a b = a -> b -> s (a, b)

data Market s a b = Market {
    _mTrials    :: Int,
    _mMatcher   :: Matcher (State s) a b,
    _mSuppliers :: ALens' s (M.IntMap a),
    _mGetSeller :: a -> Maybe Seller,
    _mDemanders :: ALens' s (M.IntMap b),
    _mGetBuyer  :: b -> Maybe Buyer
}

makeLenses ''Market

runMarket :: RSim (State s) => Market s a b -> State s ()
runMarket m = do
    sups <- use (cloneLens (m^.mSuppliers))
    dems <- use (cloneLens (m^.mDemanders))
    let sellers = M.foldl mayFolder [] (fmap getS sups)
        buyers = M.foldl mayFolder [] (fmap getB dems)
    marketLoop m sellers buyers
  where
    getS = m^.mGetSeller
    getB = m^.mGetBuyer
    mayFolder acc mb = case mb of
        Nothing -> acc
        Just b -> b:acc

marketLoop :: RSim (State s) => Market s a b -> 
    [Seller] -> [Buyer] -> State s ()
marketLoop m sells buys = do 
    shuffled <- shuffle buys (length buys)
    (sellers', buyers') <- foldM tryMatch (sells, []) shuffled
    unless (shouldStop sellers' buyers') $ 
        marketLoop m sellers' buyers'
  where
    trials      = m^.mTrials
    match       = m^.mMatcher
    suppliers   = m^.mSuppliers
    getS        = m^.mGetSeller
    demanders   = m^.mDemanders
    getB        = m^.mGetBuyer

    tryMatch (sellers, buyers) b =  do
        let (fSellers, _) = partition (\s -> (askPrice s) <= (bidPrice b)) sellers
        if null fSellers
            then return (sellers, b:buyers)
            else do
                -- take random suppliers
                rSellers <- randSim $ (\rs -> randIds rs fSellers (length fSellers) trials)
                -- choose cheapest offer and match
                let best = minimumBy (compare `on` askPrice) rSellers
                Just d <- use $ (cloneLens demanders).at (buyerId b)
                Just s <- use $ (cloneLens suppliers).at (sellerId best)
                (s', d') <- match s d
                (cloneLens demanders).ix (buyerId b) .= d'
                (cloneLens suppliers).ix (sellerId best) .= s'
                let dem = getB d'
                    sup = getS s'
                -- filter out exhausted demand/supply (no ask or pid)
                let newSups = case sup of
                        Nothing -> deleteBy ((==) `on` sellerId) best sellers
                        Just sup' -> sup':(deleteBy ((==) `on` sellerId) best sellers)
                    newDs = case dem of
                        Nothing -> buyers
                        Just dem' -> dem':buyers
                return (newSups, newDs)

    shouldStop ss ds = if (null ss) || (null ds)
        then True 
        else if (smallAsk ss) > (bigBid ds)
            then True
            else False
      where
        smallAsk s  = askPrice $ minimumBy (compare `on` askPrice) s
        bigBid d    = bidPrice $ maximumBy (compare `on` bidPrice) d



