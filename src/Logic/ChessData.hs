{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.ChessData where

import           Control.Lens
import           Control.Monad
import           Data.Array
import Database.Persist.TH


data GameStatus = Running | Finished deriving (Show, Read, Eq)

data Result = Draw | Winner Color deriving (Show, Read, Eq)
data Color = Black | White deriving (Eq, Read, Show)

switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White

data Piece = Pawn | Queen | King | Rook | Bishop | Knight deriving Eq 

data Square = None | Ent Color Piece deriving Eq

data Move = Move { _number :: Int, _orig :: (Int, Int), _dest :: (Int, Int)}
makeLenses ''Move


-- Immutable Data Type
-- convention for index: (x, y); (1, 1) is left rook of white
type Board = Array (Int, Int) Square
data ChessData = ChessData {  _status       :: GameStatus,
                              _board        :: Board,
                              _playerOnTurn :: Color,
                              _offPieces    :: [(Piece, Color)],
                              _history      :: [Move] --history, youngest move is first in list
                           }
makeLenses ''ChessData

-- Mutable Data Type
-- type BoardM s = STArray s (Int, Int) Square
-- data ChessDataM s = ChessDataM { status     :: STRef s GameStatus,
--                                board        :: BoardM s,
--                                playerOnTurn :: STRef s Color,
--                                offPieces    :: STRef s [(Piece, Color)],
--                                history      :: STRef s [Move]
--                              }

initBoard :: Board
initBoard = array ((1, 1), (8, 8)) completeBoard
    where completeBoard = firstLine White 1
                          ++ pawnLine White 2
                          ++ (foldl1 (++) [emptyLine n | n <- [3..6]])
                          ++ pawnLine Black 7
                          ++ firstLine Black 8
pawnLine :: Color -> Int -> [((Int, Int), Square)]
pawnLine c n = [((i, n), Ent c Pawn) | i <- [1..8]]
firstLine :: Color -> Int -> [((Int, Int), Square)]
firstLine c n = [((1, n), Ent c Rook),
                 ((2, n), Ent c Knight),
                 ((3, n), Ent c Bishop),
                 ((4, n), Ent c Queen),
                 ((5, n), Ent c King),
                 ((6, n), Ent c Bishop),
                 ((7, n), Ent c Knight),
                 ((8, n), Ent c Rook)]
emptyLine :: Int -> [((Int, Int), Square)]
emptyLine n = [((i, n), None) | i <- [1..8]]

newGame :: ChessData
newGame = ChessData Running initBoard White [] []

-- thawChessData :: ChessDataI -> ST s (ChessDataM s)
-- thawChessData cd = do status' <- newSTRef $ statusI cd
--                       board' <- thaw $ boardI cd
--                       playerOnTurn' <- newSTRef $ playerOnTurnI cd
--                       offPieces' <- newSTRef $ offPiecesI cd
--                       history' <- newSTRef $ historyI cd
--                       return $ ChessDataM status' board' playerOnTurn' offPieces' history'
--
-- freezeChessData :: ChessDataM s -> ST s ChessDataI
-- freezeChessData cd = do
--     status' <- readSTRef $ status cd
--     board' <- freeze $ board cd
--     playerOnTurn' <- readSTRef $ playerOnTurn cd
--     offPieces' <- readSTRef $ offPieces cd
--     history' <- readSTRef $ history cd
--     return (ChessDataI status' board' playerOnTurn' offPieces' history')


addMoveToHistory :: Move -> ChessData -> ChessData
addMoveToHistory mv = over ( history ) ((:) mv)

updatePlayerOnTurn :: ChessData -> ChessData
updatePlayerOnTurn = over playerOnTurn switchColor

updateMove :: Move -> ChessData -> ChessData
updateMove (Move _ o d) = over board (\b -> b // [(o, None), (d, b ! o)])

updateOffPieces :: Move -> ChessData -> ChessData
updateOffPieces mv cd = set offPieces (addOffPieces mv cd) cd

addOffPieces :: Move -> ChessData -> [(Piece, Color)]
addOffPieces (Move _ _ d) cd = case ((cd^.board) ! d) of
                                        None -> view offPieces cd
                                        (Ent col piece) -> (piece, col) : (cd^.offPieces)

getSquare :: (Int, Int) -> ChessData -> Square
getSquare c cd = (cd^.board) ! c


getAllPositions :: ChessData -> Color -> [(Int, Int)]
getAllPositions cd col = filter (\i -> corcol ((cd^.board) ! i)) [(x, y) | x <- [1..8], y <- [1..8]]
    where corcol = \e -> case e of (Ent boardcol _) -> boardcol == col
                                   _ -> False

-- @TODO make nicer
getKingPosition :: ChessData -> Color -> (Int, Int)
getKingPosition cd col = (filter (\i -> corking ((cd^.board) ! i)) [(x, y) | x <- [1..8], y <- [1..8]]) !! 1
    where corking = \e -> case e of (Ent col King) -> True
                                    _ -> False

-- @TODO update game status
setMove :: Move -> ChessData -> ChessData
setMove mv cd = addMoveToHistory mv $ updateMove mv $ updateOffPieces mv $ updatePlayerOnTurn cd

-- Warning: does not check if moves are legal
gameFromMoves :: [Move] -> ChessData
gameFromMoves []       = newGame
gameFromMoves (mv:mvs) = setMove mv (gameFromMoves mvs)

