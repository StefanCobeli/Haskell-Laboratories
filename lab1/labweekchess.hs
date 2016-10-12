-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck
import Data.Char



-- Exercise 8:

pic1 :: Picture
pic1 = above
	(beside knight (invert(knight)))
	(beside (invert(knight)) knight)
pic2 :: Picture
pic2 = above
	(beside knight (invert(knight)))
	(beside (flipV(invert(knight))) (flipV(knight)))
	
pic3 :: Picture
pic3 = beside pic1 pic2	


-- Exercise 9:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

whitePieces :: Picture
whitePieces = beside rook (beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook))))))


whiteRow :: Picture
whiteRow = over whitePieces otherEmptyRow

blackPieces :: Picture
blackPieces = beside (invert rook) (beside (invert knight) (beside (invert bishop) (beside (invert queen)
 (beside (invert king) (beside (invert bishop) (beside (invert knight) (invert rook)))))))


blackRow :: Picture
blackRow = over blackPieces emptyRow

-- e)


whitePawns :: Picture
whitePawns = over (repeatH 8 pawn) emptyRow

blackPawns :: Picture
blackPawns = over (repeatH 8 (invert pawn)) otherEmptyRow

populatedBoard :: Picture
populatedBoard = above blackRow (above blackPawns (above middleBoard (above whitePawns whiteRow)))



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 10:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = beside (twoAbove x) (invert (twoAbove x))

-- Exercise 11 (extra credit):
-- https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation

-- Using the "clear" Picture to skip over the board
mate :: Picture
mate = over (above clear (above (beside (repeatH 6 clear) (beside (invert queen) king)) (beside (repeatH 5 clear) (invert king)))) middleBoard

e4c5Nf3 :: Picture
e4c5Nf3 = undefined
--- "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R"
