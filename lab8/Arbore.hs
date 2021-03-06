module Arbore (Tree,
               adauga,
               cauta,
               ini,
               parcurgere)
where


data Tree a = Leaf
            | Node a (Tree a) (Tree a)
            deriving Show

-- tree for testing
root :: Tree Int
root = (Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 10 Leaf Leaf))
--        7
--      /   \
--     3     10
--    / \    
--   1   5

adauga :: Ord a => a -> Tree a -> Tree a
adauga elem Leaf             = Node elem Leaf Leaf
adauga elem (Node val lt rt) | val < elem = Node val lt (adauga elem rt)
                             | otherwise  = Node val (adauga elem lt) rt

cauta :: Ord a => a -> Tree a -> Maybe a
cauta elem Leaf             = Nothing
cauta elem (Node val lt rt) | val == elem = Just val
                            | val < elem  = cauta elem rt
                            | otherwise   = cauta elem lt


ini :: Ord a => [a] -> Tree a
ini list = foldr adauga Leaf list

-- parcurgere (ini [1,5,2,8,10,3,11,6,7]) == [1,2,3,5,6,7,8,10,11]
parcurgere :: Tree a -> [a]
parcurgere Leaf = []
parcurgere (Node val lt rt) = parcurgere lt ++ [val] ++ parcurgere rt