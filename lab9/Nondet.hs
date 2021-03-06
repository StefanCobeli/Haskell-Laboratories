module Nondet where
import Data.Complex
import Test.QuickCheck

type Nondet a = [a]

sqrts :: Floating a => a -> Nondet a
sqrts x = [y, negate y]
  where
    y = sqrt x


-- Exercise Four:  Apply the function on all input values and aggregate the results
bind :: (a -> Nondet b) -> (Nondet a -> Nondet b)
bind f roots = foldr (++) [] [f x| x <- roots]
--alternative cleaner solution -- bind f x = concat (map f x)

-- Using # instead of * for composition to avoid ambiguities
(#) :: (b -> Nondet c) -> (a -> Nondet b) -> (a -> Nondet c)
g' # f' = bind g' . f'

-- Exercise Five: Provide minimal context for the given value
unit :: a -> Nondet a
unit x = [x]

-- lift --- lifting functions
lift :: (a -> b) -> (a -> Nondet b)
lift f = unit . f


-- Solution to the quadratic equation a*x^2 + b * y + c = 0
-- delta = b^2 - 4*a*c
-- x = (-b ­+- sqrt delta) / (2*a)
solveQuadratic :: Floating a  => a -> a -> a -> Nondet a
solveQuadratic a b c = lift (/(2*a)) # lift (b+) # sqrts $ delta
  where
    delta = b*b - 4*a*c


-- Exercise Six:  Test that (for a given value x)
-- (a) f # unit = unit # f = f 
-- (b) lift g # lift f = lift (g.f)
check_unit1, check_unit2 :: (Complex Float -> Nondet (Complex Float)) -> Complex Float -> Bool
check_unit1 f x = (f # unit) x == (unit # f) x
check_unit2 f x = f x == (f # unit) x 

test_unit1, test_unit2 :: IO ()
test_unit1 = quickCheck $ check_unit1 sqrts 
test_unit2 = quickCheck $ check_unit2 sqrts 

check_lift :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
check_lift f g x = (lift f # lift g) x == lift (f . g) x

test_lift :: IO ()
test_lift = quickCheck $ check_lift (+2) (*3)


-- Exercise Ten(b): Rewrite the module to make Nondet instance of 
-- the Monad typeclass

data MyRoots a = MyR [a]
    deriving (Eq, Show)

instance Functor MyRoots where
    fmap compR (MyR roots) = MyR (map compR roots)

instance Applicative MyRoots where
    pure root               = MyR [root]
    MyR compR <*> MyR roots = MyR (concat [map comp roots | comp <- compR]) 
    
instance Monad MyRoots where
    return              = pure
    MyR roots >>= compR = foldr concRoots (MyR []) (map compR roots)
        where 
            concRoots (MyR x) (MyR y) = MyR (x++y)

            
mySqrts :: Floating a => a -> MyRoots a
mySqrts x = MyR [y, negate y]
  where
    y = sqrt x
            
mySolveQuadratic :: Floating a  => a -> a -> a -> MyRoots a
mySolveQuadratic a b c = myAdd (MyR [0] >>= return . (/(2*a))  
                            >>= (return . (b+))) (mySqrts $ delta)
                              where
                                myAdd (MyR x) (MyR y) = MyR [head x + head y, head x + last y]
                                delta = b*b - 4*a*c

-- Exercise Eleven(b):  Write the solution to the quadratic equation in do notation

