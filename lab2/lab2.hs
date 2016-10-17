-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x <- xs,
                           mod x 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | mod x 2 == 0 = (div x 2 : halveEvensRec(xs))
                     | otherwise = halveEvensRec(xs)

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens array = (halveEvens array) == (halveEvensRec array) 



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs,
                        x >= lo,
                        x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | (x <= hi && x>= lo) = (x : inRangeRec lo hi xs)
                        | otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRangeRec lo hi xs) == (inRange lo hi xs)



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs,
								x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0 = 1 + countPositivesRec xs
						 | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives = undefined



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount price = round ((fromIntegral price :: Float) * 0.9) 

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [discount(x) | x <- xs, (discount x) <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) | discount x <= 19900 = discount x + pennypincherRec xs
					| otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher = undefined



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [digitToInt x | x <- xs, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
					 | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits = undefined



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise (x:xs) = ( toUpper x : [toLower y | y <- xs] )

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec = undefined

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise = undefined



-- 7. title

-- List-comprehension version

wiseCapitalise :: String -> String
wiseCapitalise word | length word > 3 = capitalise word
					| otherwise = [toLower x | x <- word]
					
title :: [String] -> [String]
title (x:xs) = (capitalise x : map wiseCapitalise xs) 

-- Recursive version
titleRec :: [String] -> [String]
titleRec = undefined

-- mutual test
prop_title :: [String] -> Bool
prop_title = undefined

