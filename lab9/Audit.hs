module Audit where
import Data.Char
import Test.QuickCheck

type Audit a = (a,String)

-- Given a function, debug produces a function logging information 
-- about the computed value for each argument
debug :: (Show a, Show b) =>  (a -> b) -> (a -> Audit b)
debug f x = (y, show x ++ " |-> " ++ show y ++ "; ")
  where
    y = f x

ord' :: Char -> Audit Int
ord' = debug ord

chr' :: Int -> Audit Char
chr' = debug chr

-- Exercise 1
bind :: (a -> Audit b) -> (Audit a -> Audit b)
bind f (x, strx) = (y, strx ++ stry) 
    where
        (y, stry) = f x
        
--palcuie solution:  https://github.com/palcu/homework/blob/master/pd/lab8/Audit.hs
--bind :: (a -> Audit b) -> (Audit a -> Audit b)
--bind f y = (fst(f(fst(y))), snd(y) ++ snd(f(fst y)))

-- Using # instead of * for composition to avoid ambiguities
(#) :: (b -> Audit c) -> (a -> Audit b) -> (a -> Audit c)
g' # f' = bind g' . f'

-- Exercise 2
unit :: a -> Audit a
unit x = (x, "")

-- lift --- lifting functions
lift :: (a -> b) -> (a -> Audit b)
lift f = unit . f

-- Test that (for a given value x) lift g # lift f = lift (g.f) 
-- For simplicity we restrict to Float functions as in the tutorial
check_lift :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
check_lift f g x = (lift g # lift f) x == lift (g . f) x

test_lift :: IO ()
test_lift = quickCheck $ check_lift (+2) (*3)

-- Exercise Ten (a): Rewrite the module to make Audit instance of 
-- the Monad typeclass