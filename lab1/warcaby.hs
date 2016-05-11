module Warcaby where

import System.IO


type Board = [[Square]]
type Square = Maybe Piece
data Piece = Piece PColor PType 
data PColor = White | Black 
data PType = Pawn | Queen 

initialBoardStr = unlines [" b b b b"
                          ,"b b b b "
                          ," b b b b"
                          ,"        "
                          ,"        "
                          ,"w w w w "
                          ," w w w w"
                          ,"w w w w "
			]

showBoard :: Board -> String
showBoard = unlines . map showRow
  where showRow = map showSquare
showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

readBoard :: String -> Board
readBoard = map readRow . lines
  where readRow = map readSquare
readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare c   = Just (readPiece c)

showPiece :: Piece -> Char
showPiece (Piece White Pawn)   = 'w'
showPiece (Piece White Queen)  = 'W'
showPiece (Piece Black Pawn)   = 'b'
showPiece (Piece Black Queen)  = 'B'

readPiece :: Char -> Piece
readPiece 'w' = (Piece White Pawn)
readPiece 'W' = (Piece White Queen)
readPiece 'b' = (Piece Black Pawn)
readPiece 'B' = (Piece Black Queen)


let genMoves (a,b) = concat [[(a+f,b+f),(a-f,b-f)] | f <- [1..7]]
let genMoves c (a,b) = filter (\(x,y)-> and [x>=1, x<=8, y>=1, y<=8]) $ [1..2] >>= (\f -> m)
--genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]
