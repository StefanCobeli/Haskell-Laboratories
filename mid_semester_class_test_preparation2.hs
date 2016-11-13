--UNIVERSITY OF EDINBURGH
--COLLEGE OF SCIENCE AND ENGINEERING
--SCHOOL OF INFORMATICS
--Date: Monday 24th October 2011
--Duration: 35 minutes
--INFORMATICS 1 — FUNCTIONAL PROGRAMMING
--CLASS TEST

--source
--https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/classexam-2015.pdf


import Data.Char
import Test.QuickCheck


--1

--a
f :: Char -> Int
f x | '0' <= x && x <= '9' = ord x -48
    | 'a' <= x && x <= 'f' = ord x - 87
    | 'A' <= x && x <= 'F' = ord x - 55
    | otherwise = error "Invalid Hexa imput!"
 
--b

isHex :: Char -> Bool
isHex x =  '0' <= x && x <= '9'
        || 'a' <= x && x <= 'f'
        || 'A' <= x && x <= 'F'

g :: String -> Int
g xs = maximum (-1 : [f x | x <- xs, isHex x])
 

 --c
 
h :: String -> Int
h [] = (-1)
h (x:xs) | isHex x = max (f x) (h xs)
          | otherwise = h xs
          
          
--2

--a
c :: [Int] -> Int
c sequence | sequence == [] = error "Please give a nonempty sequence!"
           | otherwise = product [x-y |  (x,y) <- zip (init sequence) (tail sequence)]


--b
d :: [Int] -> Int
d [] = error "Please give a nonempty sequence!"
d [elem] = 1
d (x : (y : ys)) = (x - y) * d (y : ys)

--c 
prop_cd :: [Int] -> Bool
prop_cd sequence = null sequence || c sequence == d sequence 










