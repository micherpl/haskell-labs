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

nextMoves piece (a,b) =
	case _pColor piece of
		White -> case _pType piece of
			Pawn -> filter (\(x,y)-> and [x>=1, x<=8, y>=1, y<=8]) (concat [[(a+f,b-f),(a-f,b-f)] | f <- [1..8]])
			Queen -> filter (\(x,y)-> and [x>=1, x<=8, y>=1, y<=8]) (concat [[(a-f,b-f),(a-f,b-f)] | f <- [1..8]])
		Black -> case _pType piece of
			Pawn -> filter (\(x,y)-> and [x>=1, x<=8, y>=1, y<=8]) (concat [[(a+f,b+f),(a-f,b-f)] | f <- [1..8]])
			Queen -> filter (\(x,y)-> and [x>=1, x<=8, y>=1, y<=8]) (concat [[(a-f,b-f),(a-f,b-f)] | f <- [1..8]])

putPieceOnBoard piece (a,b) = 
    splitAt a 
 
 addPair::(Int,Int)->(Int,Int)->(Int,Int)
addPair (a,b) (c,d) = (a+c,b+d)
multPair::Int->(Int,Int)->(Int,Int)
multPair n (a,b) = (n*a,n*b)

applyAll::a->[a->a]->a
applyAll a [] = a
applyAll a (f:xs) = applyAll (f a) xs

updateList::[a]->Int->(a->a)->[a]
updateList [] _ _ = []
updateList (x:xs) 0 f = (f x):xs
updateList (x:xs) n f = x:updateList xs (n-1) f

updateMatrix::(Int, Int)->a->[[a]]->[[a]]
updateMatrix (i,j) a m = updateList m i (\z->updateList z j (const a))    
    -- PieceColor indicates whose turn it is
type State = (PieceColor, Board)

straight, diagonal::[Pos]
straight = [(0,1),(0,-1),(1,0),(-1,0)]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]

-- rough definition of moves to be refined later
moves::PieceType->[Pos]
moves Rook = straight
moves Bishop = diagonal
moves Knight = [(1,2),(2,1),(-1,2),(2,-1),(-2,1),(1,-2),(-1,-2),(-2,-1)]
moves King = straight ++ diagonal
moves Queen = straight ++ diagonal
moves Pawn = []

-- direction of play
direction::PieceColor->Int
direction White = -1
direction Black = 1

-- move generator, simple pawn, no castling
genMoves::Board->Pos->[Board]
genMoves b pos = case getSquare b pos of
                   Nothing -> [] 
                   Just p -> map (flip (movePos pos) b) $ genPieceMoves b pos p 

genPieceMoves::Board->Pos->Piece->[Pos]
genPieceMoves b pos (Piece Knight f) = [coord|v<-moves Knight, let coord = addPair pos v, notColor b f coord]
genPieceMoves b pos (Piece King f) = [coord|v<-moves King, let coord = addPair pos v, notColor b f coord]
genPieceMoves b pos (Piece Pawn f) = (filter (empty b) [addPair pos (direction f, 0)]) ++
                                     (filter (oppositePiece b f) (map (addPair pos) [(direction f, 1),(direction f, -1)]))
genPieceMoves b pos (Piece x f) = concatMap (iterateDirection 1 pos b f) (moves x)

notColor b f p      = inside p && not (hasColor f (getSquare b p))
empty b p           = inside p && Nothing == (getSquare b p)
oppositePiece b f p = inside p && hasColor (oppositeColor f) (getSquare b p) 

-- gets moving vectors for rooks, queens and bishops, the first paramater pos is the 
-- position of the piece, the second the direction to iterate at
iterateDirection::Int->Pos->Board->PieceColor->Pos->[Pos]
iterateDirection n pos b f r | outside aimsAt = []
                             | otherwise = case getSquare b aimsAt of
                                             Nothing -> aimsAt:iterateDirection (n+1) pos b f r
                                             Just (Piece _ f2) -> if f==f2 then [] else [aimsAt]
   where aimsAt = addPair (multPair n r) pos

nextStates::State->[State]
nextStates (f, b) = [(oppositeColor f, b')|pos<-colorPos f b, b'<-genMoves b pos]





let genMoves (a,b) = concat [[(a+f,b+f),(a-f,b-f)] | f <- [1..7]]
let genMoves c (a,b) = filter (\(x,y)-> and [x>=1, x<=8, y>=1, y<=8]) $ [1..2] >>= (\f -> m)
--genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]
