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
-- **************** data types *******************

data Piece = Piece {pieceType::PieceType, pieceColor::PieceColor} deriving Eq
data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving Eq
data PieceColor = Black | White deriving Eq

type Square = Maybe Piece

type Board = [[Square]]

type Pos = (Int, Int)

-- **************** output functions *******************

prettyBoard::Board->String
prettyBoard  = unlines . map (concatMap prettySquare)

prettyBoardIndent::Int->Board->String
prettyBoardIndent x = ('\n':) . concatMap ((('\n':take x (repeat ' '))++) . concatMap prettySquare)

instance Show PieceColor where
 show Black = "B"
 show White = "W"

instance Show PieceType where
 show King = "K"
 show Queen = "Q"
 show Knight = "N"
 show Rook = "R"
 show Bishop = "B"
 show Pawn = "P"

prettySquare::Square->String
prettySquare Nothing = "-- "
prettySquare (Just (Piece a f)) = show a ++ show f ++ " "

-- **************** auxiliary board functions *******************

oppositeColor::PieceColor->PieceColor
oppositeColor White = Black
oppositeColor Black = White

isEmpty::Board->Pos->Bool
isEmpty board pos = Nothing == getSquare board pos

emptySquare::Square
emptySquare = Nothing

getSquare::Board->Pos->Square
getSquare board (a, b) = board!!a!!b

updateBoard::Pos->Square->Board->Board
updateBoard = updateMatrix

deleteSquare::Pos->Board->Board
deleteSquare p = updateBoard p emptySquare

-- moves the piece at p1 to p2
movePos::Pos->Pos->Board->Board
movePos p1 p2 b = updateBoard p2 (getSquare b p1) (deleteSquare p1 b)

move::String->String->Board->Board
move p1 p2 = movePos (toPos p1) (toPos p2)

-- computes the internal representation of "a1:h8"
toPos::String->Pos
toPos [x, y] = (7 - (ord y - ord '1'), ord x - ord 'a')

outside,inside::Pos->Bool
outside (a, b) = a < 0 || b < 0 || a > 7 || b > 7

inside = not . outside

colorPos::PieceColor->Board->[Pos]
colorPos f board = [(a, b)|a<-[0..7],b<-[0..7], hasColor f (getSquare board (a,b))]

hasColor::PieceColor->Square->Bool
hasColor _ Nothing = False
hasColor f1 (Just (Piece a f2)) = f1 == f2

-- **************** some boards *******************

initialBoard, emptyBoard::Board
initialBoard = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
                [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
                [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece Queen White), Just (Piece King White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

emptyBoard = [[Nothing|_<-[1..8]]|_<-[1..8]]
 
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
genMoves :: (Enum t, Num t) => (t, t) -> Piece -> [(t, t)]
genMoves (a,b) (Piece pcolor ptype) =
 case pcolor of
     White -> concat [[(a+f,b+f),(a-f,b-f)] | f <- [1..7]]
     Black -> concat [[(a+f,b+f),(a-f,b-f)] | f <- [1..7]]
