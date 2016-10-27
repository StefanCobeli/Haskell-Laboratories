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
prop_cipher :: Int -> String -> Property
prop_cipher key word = (key >0 && key < 27) ==> decipherStr key (encipherStr key word) == normalize(word)

-- 11.
contains :: String -> String -> Bool
contains word group = 0 < length [index | index <- [0..(length word - length group)], 
                                         isPrefixOf group (drop index word)
                                 ]

-- 12.
candidates :: String -> [(Int, String)]
candidates cipherText = [(key, decipherStr key cipherText) | key <- [0..26], 
                              contains (decipherStr key cipherText) "THE" 
                              || contains (decipherStr key cipherText) "AND"
                        ]


-- Optional Material

-- 13.
splitEachFive :: String -> [String]
splitEachFive phrase | phrase == [] = []
                     | length phrase < 5 = [phrase ++ (replicate (5 - length phrase) 'X')]
                     | otherwise = [take 5 phrase] ++ (splitEachFive (drop 5 phrase))
-- 14.
prop_transpose :: String -> Property
prop_transpose phrase = ([length row| row <- matrix] == replicate (length matrix) (length (head matrix))) ==> transpose (transpose matrix) == matrix
 where matrix = splitEachFive phrase
 
-- 15.
encrypt :: Int -> String -> String
encrypt key message = concat (transpose (splitEachFive (encipherStr key message)))

-- 16.
removeFinalXs :: String -> String
removeFinalXs [] = []
removeFinalXs sequence | last sequence == 'X' = removeFinalXs (init sequence)
                       | otherwise = sequence
                       
splitIn :: String -> Int -> [String]
splitIn phrase number | phrase == [] = []
                        | length phrase < number = [phrase ++ (replicate (number - length phrase) 'X')]
                        | otherwise = [take number phrase] ++ (splitIn (drop number phrase) number )
 

decrypt :: Int -> String -> String
decrypt key cipherText = decipherStr key ( removeFinalXs (concat (transpose (splitIn cipherText ((length cipherText) `div` 5)))))

-- Challenge (Optional)

-- 17.
countFreqs :: String -> [(Char, Int)]
countFreqs phrase = [(letter, occurences) | letter <- ['A'..'z'], 
                     let occurences = (length [x | x <- phrase, x == letter]),
                     occurences > 0
                    ]                     

-- 18
getKey :: Char -> Int
getKey letter = (26 + (ord letter) - 69) `mod` 26

--freqDecipher :: String -> [String]
--freqDecipher cipherText = countFreqs (map toUpper cipherText)
