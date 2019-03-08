module Logic.Ai where

import Logic.ChessData
import Logic.ChessLegal

import Data.Array
import Control.Lens



-- the bigger the int, the better the situation for white
-- stable
gameEvaluate :: ChessData -> Int
gameEvaluate cd = foldl1 (+) [ evalSquare ((cd^.board) ! (x, y)) (x, y) | x <- [1..8], y <- [1..8]]
    where evalSquare None _ = 0
          evalSquare (Ent col pc) p = (acc col p) (pieceSquareTable pc) + (if (col==White) then 1 else -1)*(pieceValue pc)
            where acc White (x, y) = (\a -> a !! ((x-1)+((y-1)*8)))
                  acc Black (x, y) = (\a -> -1*(a !! (((9-x)-1)+(((9-y)-1)*8)))   )

-- gives value of a piece in centipawns
-- taken from the excellent https://www.chessprogramming.org/Simplified_Evaluation_Function
pieceValue :: Piece -> Int
pieceValue Pawn = 100
pieceValue Knight = 320
pieceValue Bishop = 330
pieceValue Rook = 500
pieceValue Queen = 900
pieceValue King = 20000

-- All tables taken from the excellent https://www.chessprogramming.org/Simplified_Evaluation_Function
-- NOTE: tables are mirrored with respect to website
pieceSquareTable :: Piece -> [Int]
pieceSquareTable Pawn = [0,  0,  0,  0,  0,  0,  0,  0,
                         5, 10, 10,-20,-20, 10, 10,  5,
                         5, -5,-10,  0,  0,-10, -5,  5,
                         0,  0,  0, 20, 20,  0,  0,  0,
                         5,  5, 10, 25, 25, 10,  5,  5,
                         10, 10, 20, 30, 30, 20, 10, 10,
                         50, 50, 50, 50, 50, 50, 50, 50,
                         0,  0,  0,  0,  0,  0,  0,  0]
pieceSquareTable Knight = [-50,-40,-30,-30,-30,-30,-40,-50,
                           -40,-20,  0,  5,  5,  0,-20,-40,
                           -30,  5, 10, 15, 15, 10,  5,-30,
                           -30,  0, 15, 20, 20, 15,  0,-30,
                           -30,  5, 15, 20, 20, 15,  5,-30,
                           -30,  0, 10, 15, 15, 10,  0,-30,
                           -40,-20,  0,  0,  0,  0,-20,-40,
                           -50,-40,-30,-30,-30,-30,-40,-50]
pieceSquareTable Rook = [0,  0,  0,  0,  0,  0,  0,  0,
                         5, 10, 10, 10, 10, 10, 10,  5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                         0,  0,  0,  5,  5,  0,  0,  0]
pieceSquareTable Bishop = [-20,-10,-10,-10,-10,-10,-10,-20,
                           -10,  5,  0,  0,  0,  0,  5,-10,
                           -10, 10, 10, 10, 10, 10, 10,-10,
                           -10,  0, 10, 10, 10, 10,  0,-10,
                           -10,  5,  5, 10, 10,  5,  5,-10,
                           -10,  0,  5, 10, 10,  5,  0,-10,
                           -10,  0,  0,  0,  0,  0,  0,-10,
                           -20,-10,-10,-10,-10,-10,-10,-20]
pieceSquareTable Queen = [-20,-10,-10, -5, -5,-10,-10,-20,
                          -10,  0,  0,  0,  0,  0,  0,-10,
                          -10,  0,  5,  5,  5,  5,  0,-10,
                            0,  0,  5,  5,  5,  5,  0, -5,
                           -5,  0,  5,  5,  5,  5,  0, -5,
                          -10,  0,  5,  5,  5,  5,  5,-10,
                          -10,  0,  0,  0,  0,  5,  0,-10,
                          -20,-10,-10, -5, -5,-10,-10,-20]
-- this is the king in middle game
pieceSquareTable King = [ 20, 30, 10,  0,  0, 10, 30, 20,
                          20, 20,  0,  0,  0,  0, 20, 20,
                         -10,-20,-20,-20,-20,-20,-20,-10,
                         -20,-30,-30,-40,-40,-30,-30,-20,
                         -30,-40,-40,-50,-50,-40,-40,-30,
                         -30,-40,-40,-50,-50,-40,-40,-30,
                         -30,-40,-40,-50,-50,-40,-40,-30,
                         -30,-40,-40,-50,-50,-40,-40,-30]
-- @TODO king endgame
-- [-50,-30,-30,-30,-30,-30,-30,-50
-- -30,-30,  0,  0,  0,  0,-30,-30,
-- -30,-10, 20, 30, 30, 20,-10,-30,
-- -30,-10, 30, 40, 40, 30,-10,-30,
-- -30,-10, 30, 40, 40, 30,-10,-30,
-- -30,-10, 20, 30, 30, 20,-10,-30,
-- -30,-20,-10,  0,  0,-10,-20,-30,
-- -50,-40,-30,-20,-20,-30,-40,-50]
