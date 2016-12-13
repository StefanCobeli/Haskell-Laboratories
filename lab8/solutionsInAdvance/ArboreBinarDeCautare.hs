
{--
scrieți un program Haskell care conține un modul Arbore
 care definește o librărie conținând un tip algebric
 de date Arbore a pentru reprezentarea arborilor binari
 de căutare cu date de tip a. 

Modulul trebuie să exporte tipul de date Arbore si următoarele funcții:
adauga - care dat fiind un element x si un arbore t adaugă elementul x în arborele t
cauta - care dat fiind un n element x si un arbore t spune dacă acesta se află în arbore.
fromList - care dată fiind o listă de elemente creează un arbore binar de căutare care le conține
toList - care dat fiind un arbore produce lista obținută prin parcurgerea în inordine (SRD) a arborelui.
--}


--Mai jos definim "librăria:"

--Aici este declarat modulul cerut și în paranteze se află lucrurile vizibile
--pentru cei care vor importa modulul. 
--Ex: Dacă funcția "adauga" nu s-ar afla în enumerarea de mai jos, nu s-ar putea 
--folosi în afara modulului.
module ArboreBinarDeCautare (
    Arbore,
    adauga,
    cauta,
    toList,
    fromList
   )
    where

data Arbore a = Nil 
              | Node a (Arbore a) (Arbore a)
    deriving Show

adauga :: (Ord q) => q -> Arbore q -> Arbore q
adauga leaf Nil             = Node leaf Nil Nil
adauga q (Node n arbS arbD) | q <= n    = Node n (adauga q arbS) arbD
                            | otherwise = Node n arbS (adauga q arbD)

cauta :: (Ord q) => q -> Arbore q -> Bool
cauta _ Nil                = False
cauta q (Node n arbS arbD) | q == n    = True
                           | q <  n    = cauta q arbS
                           | otherwise = cauta q arbD

toList :: (Ord q) => Arbore q -> [q]
toList Nil                = []
toList (Node x Nil Nil)   = [x]
toList (Node n arbS arbD) = toList arbS ++ [n] ++ toList arbD


--Funcțiile de mai sus sunt cam straightforward nu prea sunt multe de spus despre ele.
--Funcția de mai jos era dilema noastră. Dar este și aceasta super simplă.
fromList :: (Ord q) => [q] -> Arbore q
fromList = foldr adauga Nil 


