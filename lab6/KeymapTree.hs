-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT,
					testTree, prop_set_get, prop_toList_fromList, prop_toList_fromList_sorted                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree
del = undefined 
select = undefined
merge = undefined
filterLT = undefined
filterGT = undefined

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ lt rt) = 1 + max (depth lt) (depth rt)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k v lt rt) =  toList lt ++ [(k,v)] ++ toList rt

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right 
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get kk Leaf                        = Nothing 
get kk (Node key value left right) | kk == key = Just value
								   | kk <  key = get kk left
								   | otherwise = get kk right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v



-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
--fromList = foldr (uncurry set) Leaf
fromList [] = Leaf
fromList ((k,v):xs) = set k v (fromList xs)


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
