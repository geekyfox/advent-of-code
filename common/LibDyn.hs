module LibDyn where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M

import Debug.Trace

data Result k v w = Now w | Later k (v -> Result k v w)
type Meme k v = Result k v v

instance Functor (Result k v) where
    fmap f (Now w) = Now (f w)
    fmap f (Later k g) = Later k (\v -> fmap f (g v))

instance Applicative (Result k v) where
    pure x = Now x
    liftA2 f x y = case (x, y) of
        (Later k g, _) -> Later k (\v -> liftA2 f (g v) y)
        (_, Later k g) -> Later k (\v -> liftA2 f x (g v))
        (Now v, Now w) -> Now (f v w)

instance Monad (Result k v) where
    (Now w) >>= f = f w
    (Later k f) >>= g = Later k (\v -> f v >>= g)

data Memo k v = Memo {
    memoCache :: M.Map k v,
    memoFun :: k -> Result k v v
}

wrap :: (k -> Result k v v) -> Memo k v
wrap = Memo M.empty

remember :: (Ord k) => Memo k v -> k -> v -> Memo k v
remember m k v = m { memoCache = M.insert k v (memoCache m) }

get :: (Ord k) => Memo k v -> k -> (v, Memo k v)
get m k | (Just v) <- M.lookup k (memoCache m) = (v, m)
get m k =
    let
        (v, m2) = resolve m (memoFun m k)
    in 
        (v, remember m2 k v)

getMany :: (Ord k) => Memo k v -> [k] -> ([v], Memo k v)
getMany m [] = ([], m)
getMany m (k:ks) =
    let
        (v, m2) = get m k
        (vs, m3) = getMany m2 ks
    in
        (v:vs, m3)

evalMany :: (Ord k) => (k -> Meme k v) -> [k] -> [v]
evalMany f = fst . getMany (wrap f)

resolve :: (Ord k) => Memo k v -> Result k v w -> (w, Memo k v)
resolve m (Now w) = (w, m)
resolve m (Later k f) =
    let
        (v, m2) = get m k
    in
        resolve m2 (f v)

dyn :: k -> Result k v v
dyn k = Later k pure

dynMany :: [k] -> Result k v [v]
dynMany [] = Now []
dynMany (k:ks) = do
    v <- dyn k
    vs <- dynMany ks
    return (v:vs)

fib :: Memo Int Integer
fib = wrap f
    where
        f :: Int -> Result Int Integer Integer
        f n = do
            result <- g n
            let logMsg = "fib(" ++ show n ++ ") = " ++ show result
            trace logMsg $ return result
        g 0 = return 0
        g 1 = return 1
        g n | even n && (n > 10) = do
            let m = n `div` 2
            a <- dyn m
            b <- dyn $ m - 1
            c <- dyn $ m + 1
            return $ a * (b + c)
        g n | odd n && (n > 10) = do
            a <- dyn $ n + 1
            b <- dyn $ n - 1
            return $ a - b
        g n = do
            a <- dyn $ n - 1
            b <- dyn $ n - 2
            return $ a + b

testFib = do
    let (v, fib2) = get fib 1000
    print v

