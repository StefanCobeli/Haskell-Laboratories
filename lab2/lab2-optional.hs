-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
import Test.QuickCheck


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter index len words = [word | word <- words,
                                               index >= 0,
                                               length word > index ,
                                              (word !! index) == letter,
                                              length word == len] 

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec letter index len [] = []
crosswordFindRec letter index len [""] = []
crosswordFindRec letter index len (x:xs) | ( index >= 0
                                             &&(length x > index)
                                             && (x !! index) == letter
                                             && (length x == len) )
                                             
                                          = (x : crosswordFindRec letter index len xs)
                                         | otherwise = crosswordFindRec letter index len xs
-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter index len words = (crosswordFind letter index len words) ==
                                            (crosswordFindRec letter index len words)



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search word letter = [x | x <- [0 .. (length word - 1)],
                          word !! x == letter]

-- Recursive version

searchIndex :: String -> Char -> Int -> String -> [Int] 
searchIndex [] _ _ _ = []
searchIndex (x:xs) letter index word | ( word !! index) == letter = (index : (searchIndex xs letter (index + 1) word)  )
                                     | otherwise = searchIndex xs letter (index + 1) word

searchRec :: String -> Char -> [Int]
searchRec word letter = searchIndex word letter 0 word 

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search word letter = ( searchRec word letter == search word letter)


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec = undefined

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined

