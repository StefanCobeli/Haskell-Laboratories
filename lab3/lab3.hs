-- Declarative Programming
-- Lab 3
--


import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate 0 word = word
rotate n (x:xs) | n < 0 = error "Function only defined on positive numbers!"
				| n > (length xs + 1) = error "Function numbers shorter then the word!"
				| otherwise = rotate (n-1) (xs ++ [x])

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey key = ['A'..'Z'] `zip` (rotate key ['A'..'Z'])

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp char [] = char 
lookUp char ((k,v):xs) | char == k = v
					   | otherwise = lookUp char xs

-- 5.
encipher :: Int -> Char -> Char
encipher key char = lookUp char (makeKey key)

-- 6.
normalize :: String -> String
normalize word = [toUpper char | char <- word, isDigit char || isLetter char]

-- 7.
encipherStr :: Int -> String -> String
encipherStr key word = [encipher key letter | letter <- normalize word]  

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey dictionary = [(key, value) | (value, key) <- dictionary]

-- 9.
decipher :: Int -> Char -> Char
decipher key char = lookUp char (reverseKey $ makeKey key )

decipherStr :: Int -> String -> String
decipherStr key secret = [decipher key letter | letter <- normalize secret] 

-- 10.
prop_cipher :: Int -> String -> Bool
prop_cipher key word = decipherStr key (encipherStr key word) == normalize(word)

-- 11.
contains :: String -> String -> Bool
contains (first:word) group | length group > length word  + 1 = False
							| group `isPrefixOf` (first:word) = True
							| otherwise = contains word group

-- 12.
candidates :: String -> [(Int, String)]
candidates cipherText = [(key, decipherStr key cipherText) | key <- [0..26], 
										  contains (decipherStr key cipherText) "THE" 
										  || contains (decipherStr key cipherText) "AND"
						]

