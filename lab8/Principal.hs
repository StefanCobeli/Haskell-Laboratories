module Principal where

import Arbore

main :: IO [Int]
main  =  do 
            line <- getLine
            let x = map read (words line) :: [Int]
            return $ parcurgere $ ini x
