-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Go x)      = [Go x]
split (Turn x)    = [Turn x]
split Sit         = []
split (c1 :#: c2) = split c1 ++ split c2

-- 1b. join
join :: [Command] -> Command
join []           = Sit
join (Sit : cmds) = join cmds
join (cmd : cmds) = cmd :#: (join cmds)

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent c1 c2 = split c1 == split c2

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join cmd = equivalent cmd (join (split cmd))

prop_split :: Command -> Bool
prop_split cmd = (length [c | c <- split cmd, isComplexCommand c]) == 0
-- = not (Sit `elem` (split cmd)) &&
    
isComplexCommand :: Command -> Bool
isComplexCommand (Go _)   = False 
isComplexCommand (Turn _) = False 
isComplexCommand _        = True
    
                
                
-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy nb cmd =  foldr (:#:) Sit (replicate nb cmd)
--join (foldr (++)  (replicate nb [cmd]))

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon nb = copy 5 (Go nb :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon length size = copy size (Go length :#: Turn (360 / (fromIntegral size)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _           = Sit
spiral 0 _ _ _           = Sit
spiral side n step angle = Go side :#: Turn angle :#: spiral (side + step) (n-1) step angle 


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise cmd = collapse (excludeStops (collapse (excludeStops cmd)))

excludeStops :: Command -> [Command]
excludeStops = (filter (/= Turn 0)) . (filter (/= Go 0)) . (filter (/= Sit)) . split

collapse :: [Command] -> Command
collapse []                             = Sit
collapse [cmd]                          = cmd
collapse ((Go x) : ((Go y) : cmds))     | x == -y   = collapse cmds
                                        | otherwise = collapse ((Go (x + y)) : cmds)
collapse ((Turn x) : ((Turn y) : cmds)) | x == -y   = collapse cmds
                                        | otherwise = collapse ((Turn (x + y)) : cmds)
collapse (cmd : cmds)                   = cmd :#: collapse cmds 
-- L-Systems

-- 5. arrowhead
--angle:      60
--start:      f
--rewrite:    f → g+f+g
--            g → f-g-f

arrowhead :: Int -> Command
arrowhead steps = f steps
    where f 0 = GrabPen salmon :#: Go 10
          f x = g (x-1) :#: n :#: f (x-1) :#: n :#: g (x-1)
          g 0 = GrabPen forest :#: Go 10
          g x = f (x-1) :#: p :#: g (x-1) :#: p :#: f (x-1)
          n   = Turn 45
          p   = Turn (-45)

-- 6. snowflake
--angle:      60
--start:      f- -f- -f- -
--rewrite:    f → f+f- -f+f

snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where f 0 = GrabPen forest :#: Go 10
          f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
          n   = Turn 75
          p   = Turn (-75)

-- 7. hilbert
--angle:      90
--start:      l
--rewrite:    l → +rf-lfl-fr+
--            r → -lf+rfr+fl-
hilbert :: Int -> Command
hilbert = l
    where l 0 = Sit
          l x = p :#: r (x-1) :#: f :#: n :#: l (x-1) :#: f :#: l (x-1) :#: n :#: f :#: r (x-1) :#: p
          r 0 = Sit
          r x = n :#: l (x-1) :#: f :#: p :#: r (x-1) :#: f :#: r (x-1) :#: p :#: f :#: l (x-1) :#: n
          f   = GrabPen midnight :#: Go 10
          n   = Turn 90
          p   = Turn (-90)

-- 8. Peano-Gosper
--angle:      60
--start:      f
--rewrite:    f → f+g++g-f- -ff-g+
--g → -f+gg++g+f- -f-g
  
peanoGosper :: Int -> Command
peanoGosper = f
    where f 0 = GrabPen midnight :#: Go 10
          f x = f (x-1) :#: n :#: g (x-1) :#: n :#: n :#: g (x-1) :#: p :#: f (x-1) :#: p :#: p :#: f (x-1) :#: f (x-1) :#: p :#: g (x-1) :#: n
          g 0 = GrabPen salmon :#: Go 10
          g x = p :#: f (x-1) :#: n :#: g (x-1) :#: g (x-1) :#: n :#: n :#: g (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: p :#: g (x-1)
          n   = Turn 60
          p   = Turn (-60)

cross :: Int -> Command
cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x :#: n
    where f 0 =  GrabPen forest :#: Go 10
          f x = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) 
          n   = Turn 90
          p   = Turn (-90)

branch :: Int -> Command
branch x = g x
    where g 0 = GrabPen red :#: Go 10
          g x = f (x-1) :#: p :#: Branch (Branch (g (x-1)) :#: n :#: g (x-1)) :#: f (x-1) :#: Branch (n :#: f (x-1) :#: g (x-1)) :#: p :#: g (x-1)
          f 0 = GrabPen salmon :#: Go 10
          f x = f (x-1) :#: f (x-1)
          n   = Turn 22.5
          p   = Turn(-22.5)

thirtytwo :: Int -> Command
thirtytwo x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x
    where f 0 = GrabPen midnight :#: Go 10.0
          f x =  p :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: n
          n   = Turn 90
          p   = Turn (-90)

  
main :: IO ()
main = display (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)
