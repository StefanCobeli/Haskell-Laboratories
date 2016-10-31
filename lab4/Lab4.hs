-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 22/23 Oct.

import Data.Char
import Test.QuickCheck
import Data.List


-- 1. Map
-- a. (4 simboluri)
uppers :: String -> String
uppers word = map toUpper word

-- b. (7 simboluri)
doubles :: [Int] -> [Int]
doubles numbers = map (*2) numbers 

-- c. (10 simboluri)
penceToPounds :: [Int] -> [Float]
penceToPounds = undefined

-- d. (11 simboluri)
uppers' :: String -> String
uppers' word | word == "" = ""
             | otherwise = [toUpper letter | letter <- word]

-- (8 simboluri)
prop_uppers :: String -> Bool
prop_uppers word = uppers word == uppers' word



-- 2. Filter
-- a. (4 simboluri)
alphas :: String -> String
alphas sequence = filter isLetter sequence 

-- b. (8 simboluri)
rmChar ::  Char -> String -> String
rmChar letter sequence = filter (/= letter) sequence

-- c. (8 simboluri)
above :: Int -> [Int] -> [Int]
above number sequence = filter (> number) sequence

-- d. (13 simboluri)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals pairs = filter (\x -> fst x /= snd x) pairs

-- e. (15 simboluri)
rmCharComp :: Char -> String -> String
rmCharComp letter sequence | sequence == "" = ""
                           | otherwise = [x | x <- sequence, x /= letter]

-- (11 simboluri)
prop_rmChar :: Char -> String -> Bool
prop_rmChar letter sequence = rmChar letter sequence == rmCharComp letter sequence



-- 3. Comprehensions vs. map & filter
-- a. 
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

-- (7 simboluri)
upperChars' :: String -> String
upperChars' word = map toUpper (filter isLetter word)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b. 
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- (13 simboluri)
largeDoubles' :: [Int] -> [Int]
largeDoubles' sequence = map (2*) (filter (>3) sequence)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

-- (11 simboluri)
reverseEven' :: [String] -> [String]
reverseEven' sequence = map reverse (filter (\x -> even (length x)) sequence)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

-- (7 simboluri)
productFold :: [Int] -> Int
productFold sequence = foldr (*) 1 sequence 

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.  (16 simboluri)
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x : xs) = x && andRec xs 

-- (7 simboluri)
andFold :: [Bool] -> Bool
andFold list = foldr (&&) True list

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.  (17 simboluri)
concatRec :: [[a]] -> [a]
concatRec [] = [] 
concatRec (x : xs) = x ++ concatRec xs

-- (8 simboluri)
concatFold :: [[a]] -> [a]
concatFold list = foldr (++) [] list

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.  (17 simboluri)
rmCharsRec :: String -> String -> String
rmCharsRec _ [] = []
rmCharsRec [] word = word 
rmCharsRec (first : badLetters) word = rmCharsRec badLetters (rmChar first badLetters)

-- (6 simboluri)
rmCharsFold :: String -> String -> String
rmCharsFold badLetters word = foldr rmChar word badLetters

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a. (10 simboluri)
uniform :: [Int] -> Bool
uniform [] = True
uniform sequence = all (==0) [x - sequence !! 0 | x <- sequence]

-- b. (	 simboluri)
valid :: Matrix -> Bool
valid matrix = uniform (map length matrix)

-- 6.

-- 7.  (22 simboluri + 19 simboluri)  cu tot cu tratarea erorilor
plusRow :: [Int] -> [Int] -> [Int]
plusRow row1 row2 | length row1 == length row2 = zipWith (+) row1 row2
				  |	otherwise = error "The matrix does not have the same type!"

plusM :: Matrix -> Matrix -> Matrix
plusM matrix1 matrix2 = zipWith plusRow matrix1 matrix2  

-- 8. (23 simboluri + 15 simboluri)  cu tot cu tratarea erorilor  
dot :: [Int] -> [Int] -> Int
dot vector1 vector2 | length vector1 == length vector2 =  foldr (+) 0 (zipWith (*) vector1 vector2)
                    | otherwise = error "The matrix does not have the same type!"
                    
timesM :: Matrix -> Matrix -> Matrix
timesM matrix1 matrix2 = foldr (++) [] [[map (dot row) (transpose matrix2)]| row <- matrix1] 


