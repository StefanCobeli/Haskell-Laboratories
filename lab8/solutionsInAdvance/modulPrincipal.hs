
{-- EXERCISE:
scrieți un modul Principal, executabil,
care folosește funcțiile din modulul Arbore,
pentru a citi de la tastatură o listă de numere,
separate prin spațiu, aflate pe aceeași line,
și afișează numerele citite în ordine crescătoare,
separate prin spațiu.
--}


{--
    If you want to run the application,
    load "modulPrincipal.hs" in "ghci",
    type in "main", press Enter and then
    you should introduce some numbers,
    separated by space.
    
    Example: 
    *Main> main
    1 10 2 5 7
    
    You should receive the output:
    1 2 5 7 10
    
    P.S.
    Please give a reasonable input. We do not have error handling.
--}

--Așa importăm implementarea funcțiilor și tipurilor din modulul "ArboreBinarDeCautare":
import ArboreBinarDeCautare

--Așa se definește funcția main, pe care o rulează haskell-ul
main :: IO ()
main = do
    line <- getLine --aici ia inputul de sub formă de String 
    (putStrLn . formatNumbersToString . toList . fromList . getNumbersFromInput) line
--linia dubioasă de mai sus ia inputul, 
--1. îl spltuiește la întâlnirea spațiilor și îl transformă în array de Int, 
--2. îl transformă în arbore binar, 
--3. îl transformă înpoi în listă, 4. îl formatează și 5. îl printează.
    
    
--funcție care transformă inputul în array de Int
getNumbersFromInput :: String -> [Int]
getNumbersFromInput input = [read x :: Int | x <- words input]

--Transformă o listă de numere în String cum se dorește în cerință.
formatNumbersToString :: [Int] -> String
formatNumbersToString []       = ""
formatNumbersToString [x]      = show x 
formatNumbersToString (x : xs) = show x ++ [' '] ++ formatNumbersToString xs