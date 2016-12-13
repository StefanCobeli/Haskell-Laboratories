--UNIVERSITY OF EDINBURGH
--COLLEGE OF SCIENCE AND ENGINEERING
--SCHOOL OF INFORMATICS
--Date: Tuesday 27th October 2015
--Duration: 35 minutes
--INFORMATICS 1 — FUNCTIONAL PROGRAMMING
--CLASS TEST

--source
--https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/classexam-2015.pdf


--1. (a) Define a function count :: String -> Int that counts the number of characters
--in a string that are either upper case or digits. For example:
--count "Inf1-FP" = 4 count "" = 0
--count "none here" = 0 count "5HOU7" = 5
--Your definition may use basic functions, list comprehension, and library functions,
--but not recursion.
import Data.Char
import Test.QuickCheck
import Data.List



--1
count :: String -> Int
count string = length [letter | letter <- string,
						isUpper letter || isDigit letter ]

countRec :: String -> Int
countRec [] = 0
countRec (x:xs) | isUpper x || isDigit x = 1 + countRec xs
				| otherwise = countRec xs
				
prop_count :: String -> Bool
prop_count xs = countRec xs == count xs 

test_1c = quickCheck prop_count

--2

isNext :: Int -> Int -> Bool
isNext a b | (even a && b == a `div` 2) || (odd a && b == 3 * a + 1) = True
		   | otherwise = False
		   
		   
collatz :: [Int] -> Bool
collatz [] = True
collatz sequence = length [x | x <- [0..(length sequence - 2)], 
			(init sequence !! x )`isNext` (tail sequence !! x)]
			== length sequence - 1

collatzRec :: [Int] -> Bool
collatzRec [] = True
collatzRec [element] = True
collatzRec (first : (second : rest)) = 
		isNext first second && collatzRec (second : rest)

