-- Informatics 1 - Functional Programming 
-- Tutorial 5
--
-- Due: the tutorial of week 7 (5/6 November)


import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Data.Function
import Data.Bool
-- Warmup exercises

-- The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange sort id) | sort `elem` ["Tarocco", "Moro", "Sanguinello"] = True
isBloodOrange _ = False


-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments fruits = length (filter (== True ) (map isBloodOrange fruits))

-- 3.
hasWorm :: Fruit -> Bool
hasWorm (Apple _ ans) = ans
hasWorm _ = False

worms :: [Fruit] -> Int
worms fruits = length (filter (==True) (map hasWorm fruits))

-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
		  | Prop :->: Prop
		  | Prop :<->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"


-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  not (eval e p) || eval e q
eval e (p :<->: q)    =  eval e p == eval e q 

-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

-- 4.
p1 = ((Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q"))
p2 = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not (Var "Q"))) 
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&:
	(((Not (Var "P")) :|: (Not (Var "Q"))) :&: 
	(Not (Var "P")) :|: (Not (Var "R")) )

--((P & (Q ∨ R)) & (((¬P ) ∨ (¬Q)) & ((¬P ) ∨ (¬R))))
--((Var "P" :&: Not (Var "Q")) :&: (Var "Q" :|: Var "P"))

-- 5. 
tautology :: Prop -> Bool
tautology proposition = and (
		map ((flip eval) proposition)  (envs (names proposition))
		) 

prop_taut1 :: Prop -> Bool
prop_taut1 proposition = (tautology proposition) 
			||  (satisfiable (Not proposition))

prop_taut2 :: Prop -> Bool
prop_taut2 proposition = (tautology (Not proposition)) 
			||  (satisfiable  proposition)


-- 6.
p4 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")
p5 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: (Not (Var "Q")))
p6 = (Var "P" :<->: Var "Q") :&:
	 (Var "P" :&: (Not (Var "Q"))) :|:
	 ((Not (Var "P")) :&: (Var "Q"))

-- 7.
equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2 = 
	map ((flip eval) prop1)  (envs (names (prop1 :&: prop2)))
	==
	map ((flip eval) prop2)  (envs (names (prop1 :&: prop2)))

equivalent' :: Prop -> Prop -> Bool
equivalent' prop1 prop2 = tautology (prop1 :<->: prop2)

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent prop1 prop2 = equivalent prop1 prop2 
							==
							equivalent prop2 prop1


-- 8.
--nub function removes duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []
subformulas :: Prop -> [Prop]
subformulas (T)         = [T]
subformulas (F)         = [F]
subformulas (Var x)     = [Var x]
subformulas (Not p)     = nub ([Not p] ++ subformulas p)
subformulas (p :|: q)   = nub ([p :|: q] ++ subformulas p ++ subformulas q)
subformulas (p :&: q)   = nub ([p :&: q] ++ subformulas p ++ subformulas q)
subformulas (p :->: q)  = nub ([p :->: q] ++ subformulas p ++ subformulas q)
subformulas (p :<->: q) = nub ([p :<->: q] ++ subformulas p ++ subformulas q)




-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
