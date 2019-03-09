module Logic.Ai where

import Logic.ChessData
import Logic.ChessLegal

import Data.Array
import Control.Lens
import Data.Foldable

data AIDiff = Random | Easy deriving (Show, Read, Eq, Enum)

optimisationDirection :: Color -> Int
optimisationDirection Black = -1
optimisationDirection White = 1

-- negamax :: Int -> ChessData -> (Move, Int)
-- negamax n _
--     | n < 0 = error "Negamax: Cannot go below search depth of 0"
-- negamax 0 cd = maximumBy maxtuple $ fmap (\m -> (m, (optimisationDirection (cd^.playerOnTurn)) * (gameEvaluate (setMove m cd)))) (allMovesForPlayer (cd^.playerOnTurn) cd)
-- negamax n cd = maximumBy maxtuple $ fmap (opti . negamaxmapper) ((allMovesForPlayer (cd^.playerOnTurn)) cd)
--     where negamaxmapper = (\m -> negamax (n-1) (setMove m cd))
--           opti = \(c, i) -> (c, optimisationDirection (cd^.playerOnTurn)*i)

maxtuple :: (a, Int) -> (a, Int) -> Ordering
maxtuple (_, i) (_, j) = compare i j

-- minmax :: Int -> Color -> ChessData -> Int
-- minmax 0 c cd = (optimisationDirection col) * (gameEvaluate cd)
-- minmax n c cd = case (allMovesForPlayer (cd^.playerOnTurn) cd) of
--                     [] -> (optimisationDirection c) * (optimisationDirection (cd^.playerOnTurn)) (-1000000000)
--                     mvs -> $ fmap (minmax (n-1)) mvs

minmaxRankings :: ChessData -> [(Move, Int)]
minmaxRankings cd =  (fmap (\m -> (m, (minmax' 2 (cd^.playerOnTurn)) $ setMove m cd)) (allMovesForPlayer (_playerOnTurn cd) cd))

-- this is a stable function
bestMove :: AIDiff -> ChessData -> Move
bestMove _ cd = fst $ maximumBy maxtuple (minmaxRankings cd)

movePair :: ChessData -> (Move, Int)
movePair cd = maximumBy maxtuple (minmaxRankings cd)

-- White is the player for maximisation
minmax :: Int -> ChessData -> Int
minmax 0 cd = (gameEvaluate cd)
minmax n cd = case (allMovesForPlayer (cd^.playerOnTurn) cd) of
                    [] -> (optimisationDirection (cd^.playerOnTurn)) * (-1000000000)
                    mvs -> (optifun (cd^.playerOnTurn)) $ fmap (\m -> minmax (n-1) (setMove m cd)) mvs
    where optifun c = if c==White then maximum else minimum 

minmax' :: Int -- ^ search depth
            -> Color -- ^ color of the player which is trying to optimize his move
            -> ChessData -- ^ chess situation
            -> Int -- ^ index for how good the situation is
minmax' 0 maxplayer cd = (optimisationDirection maxplayer) * (gameEvaluate cd)
minmax' n maxplayer cd = case (allMovesForPlayer (cd^.playerOnTurn) cd) of
                            [] -> (optimisationDirection maxplayer) * (optimisationDirection (cd^.playerOnTurn)) * (-1000000000)
                            mvs -> (optifun (cd^.playerOnTurn)) $ fmap (\m -> minmax' (n-1) maxplayer (setMove m cd)) mvs
    where optifun c = if c==maxplayer then maximum else minimum 

-- the bigger the int, the better the situation for white
-- stable
gameEvaluate :: ChessData -> Int
gameEvaluate cd = sum [ evalSquare ((cd^.board) ! (x, y)) (x, y) | x <- [1..8], y <- [1..8]]
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
