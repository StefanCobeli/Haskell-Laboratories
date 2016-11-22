-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 12/13 November

import System.Random


-- Importing the keymap module

import KeymapTree

import Test.QuickCheck
-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen products = maximum (map (length . fst . snd) products) 
                          

formatLine :: Int -> (Barcode, Item) -> String
formatLine l (code, (name, typ))= code ++ "..." ++ name ++ (3 + l - length name) `replicate` '.' ++ typ

showCatalogue :: Catalogue -> String
showCatalogue c = unlines (map 
		(formatLine (longestProductLen (toList c)))
		(toList c)) 
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x)  = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe x  = Just (head x)

catMaybes :: [Maybe a] -> [a]
catMaybes items = [x | (Just x) <- items]

catMaybes2 :: [Maybe a] -> [a]
catMaybes2 = concat . map maybeToList

prop_cat :: Eq a => [Maybe a] -> Bool
prop_cat list = catMaybes list == catMaybes2 list

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems list c = catMaybes (map (flip get c) list) 






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)