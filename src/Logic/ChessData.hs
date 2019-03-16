{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

{-|
Module      : Logic.ChessData

Contains all basic data structures necessary for the chess game
-}

module Logic.ChessData where

import           Control.Lens
import           Data.Array
import           Control.DeepSeq
import           GHC.Generics (Generic)

--------------------------------------------------------
-- Data types
--------------------------------------------------------

-- | defines possible game stati; determines game interaction
data GameStatus = Running | Finished Result deriving (Show, Read, Eq, Generic, NFData)
data Result = Draw | Winner Color deriving (Show, Read, Eq, Generic, NFData)

-- | Colors in chess game
data Color = Black | White deriving (Show, Eq, Read, Generic, NFData)

switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White

-- | defines type for board state information
-- convention for index: (x, y); (1, 1) is left rook of white
type Board = Array (Int, Int) Square
data Square = None | Ent Color Piece deriving (Show, Eq, Generic, NFData)
data Piece = Pawn | Queen | King | Rook | Bishop | Knight deriving (Show, Eq, Generic, NFData)

data Move = Move { _number :: Int, -- ^ number must be consistent with position in history
                   _orig :: (Int, Int), _dest :: (Int, Int)} deriving (Show, Read, Eq, Generic, NFData)
makeLenses ''Move


-- | ChessData is the base data type for all Chess operations
-- NOTE: a lot of the information seems unnecessary but is required by some chess rules (for example: history for en-passant capturing, offpieces after pawn promotion, status for succeeding)
data ChessData = ChessData {  _status       :: GameStatus,
                              _board        :: Board,
                              _playerOnTurn :: Color,
                              _offPieces    :: [(Piece, Color)],
                              _history      :: [Move] --history, youngest move is first in list
                           } deriving (Show, Generic, NFData)
makeLenses ''ChessData

--------------------------------------------------------
-- ChessData initialization

newGame :: ChessData
newGame = ChessData Running initBoard White [] []

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

--------------------------------------------------------
-- Update helper functions
--------------------------------------------------------

addMoveToHistory :: Move -> ChessData -> ChessData
addMoveToHistory mv = history%~((:) mv)

updatePlayerOnTurn :: ChessData -> ChessData
updatePlayerOnTurn = playerOnTurn%~switchColor

updateMove :: Move -> ChessData -> ChessData
updateMove (Move _ o d) = board%~(\b -> b // [(o, None), (d, b ! o)])

updateOffPieces :: Move -> ChessData -> ChessData
updateOffPieces mv cd = offPieces .~ (addOffPieces mv cd) $ cd

addOffPieces :: Move -> ChessData -> [(Piece, Color)]
addOffPieces (Move _ _ d) cd = case ((cd^.board) ! d) of
                                        None -> cd^.offPieces
                                        (Ent col piece) -> (piece, col) : (cd^.offPieces)


--------------------------------------------------------
-- Convinience & special getter functions
--------------------------------------------------------

getSquare :: (Int, Int) -> ChessData -> Square
getSquare c cd = (cd^.board) ! c

getAllPositions :: ChessData -> Color -> [(Int, Int)]
getAllPositions cd col = filter (\i -> corcol ((cd^.board) ! i)) [(x, y) | x <- [1..8], y <- [1..8]]
    where corcol = \e -> case e of (Ent boardcol _) -> boardcol == col
                                   _                -> False

-- @TODO make nicer
getKingPosition :: ChessData -> Color -> (Int, Int)
getKingPosition cd col = (filter (\i -> corking ((cd^.board) ! i)) [(x, y) | x <- [1..8], y <- [1..8]]) !! 0
    where corking = \e -> case e of (Ent c King) -> c == col
                                    _            -> False
