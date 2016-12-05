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
copy = undefined

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon = undefined

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon = undefined



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral = undefined


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = undefined



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

main :: IO ()
main = display (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)
