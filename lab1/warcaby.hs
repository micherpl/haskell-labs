module Warcaby where

import System.IO


type Board = [[Square]]
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
