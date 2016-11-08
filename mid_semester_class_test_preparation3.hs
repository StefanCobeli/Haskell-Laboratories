--UNIVERSITY OF EDINBURGH
--COLLEGE OF SCIENCE AND ENGINEERING
--SCHOOL OF INFORMATICS
--Date: Monday 21st October 2013
--Duration: 35 minutes
--INFORMATICS 1 — FUNCTIONAL PROGRAMMING
--CLASS TEST

--source
--https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams

--13:00 Start

import Data.Char
import Test.QuickCheck
import Data.List



--1

--a
countIn :: Char -> String -> Int
countIn letter word = length [x | x <- word, x == letter]

f :: Char -> Int 
f letter | isUpper letter = 2 + 8 * countIn letter "HASKEL"  
		 | isLower letter = 1 + 4 * countIn letter "haskel"
		 | otherwise = 0
		 
--b --Not respected with list comperhesion
g :: String -> Int
g xs = product( filter (>0) (map f xs)) 


--c
h :: String -> Int
h [] = 1
h (x:xs) | f x > 0 = f x * h xs
		 | otherwise = h xs


		 
--2

--a
c :: String -> String -> String
c first second = [x | ((x,y),z) <- zip (zip first [0..]) second, 
					first !! y == second !! y]

					
--b
d :: String -> String -> String
d [] _ = []
d _ [] = []
d (x:xs) (y:ys) | x == y = x : d xs ys
				| otherwise = d xs ys
				

--c
prop_cd :: String -> String -> Bool
prop_cd first second = c first second == d first second


--13:27 finish










