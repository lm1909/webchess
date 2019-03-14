module Logic.Ai where

import           Logic.ChessData
import           Logic.ChessLegal

import           Control.Lens
import           Data.Array
import           Data.Foldable
import           Data.List
import           Data.Ord
import           Data.Tree


--------------------------------------------------------
-- Interface
--------------------------------------------------------

data AIDiff = Random | Easy | Medium deriving (Show, Read, Eq, Enum, Bounded)

-- this is a stable function
bestMove :: AIDiff -> ChessData -> Move
bestMove Easy cd   = fst $ maximumBy maxtuple (minmaxRankings cd)
bestMove Medium cd = parallel_DynPrun_AlphaBeta cd
bestMove Random cd = undefined -- @TODO

--------------------------------------------------------
-- General
--------------------------------------------------------

gameTree :: ChessData -> Tree ChessData
gameTree cd = Node cd (fmap gameTree (allStates cd))
    where allStates cd = fmap (\m -> setMove m cd) (allMovesForPlayer (cd^.playerOnTurn) cd)

recdepth :: Int -> (ChessData -> Bool) -> Tree ChessData -> Tree ChessData
recdepth 0 d (Node cd children)
    | d cd = Node cd (fmap (recdepth 0 d) children)
    | otherwise = Node cd []
recdepth n d (Node cd children) = Node cd (fmap (recdepth (n-1) d) children)

--------------------------------------------------------
-- Optimized alpha-beta-Search
--------------------------------------------------------

parallel_DynPrun_AlphaBeta :: ChessData -> Move
parallel_DynPrun_AlphaBeta cd = snd $ maximumBy (comparing fst) $ fmap (\(cd, m) -> (dynprunAlphaBeta 5 (switchColor $ cd^.playerOnTurn) cd, m)) (fmap (\m -> (setMove m cd, m)) (allMovesForPlayer (cd^.playerOnTurn) cd))

dynprunAlphaBeta :: Int -> Color -> ChessData -> Int
dynprunAlphaBeta d col = alphabetaMax . horizontalprune 2 . orderhigher . fmap (\cd -> (optimisationDirection col) * (gameEvaluate cd)) . recdepth d quiescence . gameTree

-- NOTE: only act on ordered tree to not detriment move quality
horizontalprune :: Int -> Tree a -> Tree a
horizontalprune p (Node v children) = Node v (fmap (horizontalprune p) (take ((length children) `quot` p) children))

-- @TODO implement a quiescence search (this is quite a lot of work)
-- see https://www.chessprogramming.org/CPW-Engine_quiescence
quiescence :: ChessData -> Bool
quiescence _ = False -- @TODO

alphabetaMax :: Tree Int -> Int
alphabetaMax = maximum . mmMax'
alphabetaMin :: Tree Int -> Int
alphabetaMin = minimum . mmMin'

mmMax' :: Tree Int -> [Int]
mmMax' (Node v []) = [v]
mmMax' (Node _ children) = mapmin (fmap mmMin' children)
    where mapmin :: [[Int]] -> [Int]
          mapmin (l:ls) = (minimum l) : (alphacut (minimum l) ls)
          -- note mapmin [] is not possible, as it is never called as that

          alphacut :: Int -> [[Int]] -> [Int]
          alphacut _ [] = []
          alphacut alpha (x:xs)
                | minleq alpha x = alphacut alpha xs
                | otherwise = ((minimum x) : alphacut (minimum x) xs)

          minleq :: Int -> [Int] -> Bool
          minleq _ []         = False
          minleq alpha (x:xs) = (x <= alpha) || (minleq alpha xs)

mmMin' :: Tree Int -> [Int]
mmMin' (Node v []) = [v]
mmMin' (Node _ children) = mapmax (fmap mmMax' children)
    where mapmax :: [[Int]] -> [Int]
          mapmax (l:ls) = (maximum l) : (betacut (maximum l) ls)
          -- note mapmax [] is not possible, as it is never called as that

          betacut :: Int -> [[Int]] -> [Int]
          betacut _ [] = []
          betacut beta (l:ls)
            | maxgeq beta l = betacut beta ls
            | otherwise = (maximum l) : (betacut (maximum l) ls)

          -- ^ sees if the maximum of the given list is greater or equal the given bound
          maxgeq :: Int -> [Int] -> Bool
          maxgeq _ []        = False
          maxgeq beta (x:xs) = (x >= beta) || (maxgeq beta xs)


orderhigher :: Tree Int -> Tree Int
orderhigher (Node v children) = Node v (sortBy (flip (comparing (\(Node v _) -> v))) (map orderlower children))

orderlower :: Tree Int -> Tree Int
orderlower (Node v children) = Node v (sortBy (comparing (\(Node v _) -> v)) (map orderhigher children))

--------------------------------------------------------
-- Naive MiniMax implemenation
--------------------------------------------------------

maxtuple :: (a, Int) -> (a, Int) -> Ordering
maxtuple (_, i) (_, j) = compare i j

minmaxRankings :: ChessData -> [(Move, Int)]
minmaxRankings cd =  (fmap (\m -> (m, (minmax' 2 (cd^.playerOnTurn)) $ setMove m cd)) (allMovesForPlayer (_playerOnTurn cd) cd))


movePair :: ChessData -> (Move, Int)
movePair cd = maximumBy maxtuple (minmaxRankings cd)

minmax' :: Int -- ^ search depth
            -> Color -- ^ color of the player which is trying to optimize his move
            -> ChessData -- ^ chess situation
            -> Int -- ^ index for how good the situation is
minmax' 0 maxplayer cd = (optimisationDirection maxplayer) * (gameEvaluate cd)
minmax' n maxplayer cd = case (allMovesForPlayer (cd^.playerOnTurn) cd) of
                            [] -> (optimisationDirection maxplayer) * (optimisationDirection (cd^.playerOnTurn)) * (-1000000000)
                            mvs -> (optifun (cd^.playerOnTurn)) $ fmap (\m -> minmax' (n-1) maxplayer (setMove m cd)) mvs
    where optifun c = if c==maxplayer then maximum else minimum


--------------------------------------------------------
-- * Parallel Tree based MinMax
--------------------------------------------------------

minmax :: Color -> Int -> ChessData -> (Int, Move)
minmax col d cd = fmap (\m -> (minmaxeval col d (setMove m cd), m)) (allMovesForPlayer (cd^.playerOnTurn) cd)

minmaxeval :: Color -> Int -> ChessData -> Int
minmaxeval col d = mmMax . (fmap (\cd -> (optimisationDirection col) * gameEvaluate cd)) . recdepth d quiescence . gameTree


mmMax :: Tree Int -> Int
mmMax (Node v []) = v
mmMax (Node v children) = maximum (fmap mmMin children)

mmMin :: Tree Int -> Int
mmMin (Node v []) = v
mmMin (Node v children) = minimum (fmap mmMax children)

--------------------------------------------------------
-- * Instantanious Evalutation functions
--------------------------------------------------------

optimisationDirection :: Color -> Int
optimisationDirection Black = -1
optimisationDirection White = 1

-- the bigger the int, the better the situation for white
-- stable
gameEvaluate :: ChessData -> Int
gameEvaluate cd = sum [ evalSquare ((cd^.board) ! (x, y)) (x, y) | x <- [1..8], y <- [1..8]]
    where evalSquare None _ = 0
          evalSquare (Ent col pc) p = (acc col p) (pieceSquareTable pc cd) + (if (col==White) then 1 else -1)*(pieceValue pc)
            where acc White (x, y) = (\a -> a !! ((x-1)+((y-1)*8)))
                  acc Black (x, y) = (\a -> -1*(a !! (((9-x)-1)+(((9-y)-1)*8)))   )

-- gives value of a piece in centipawns
-- taken from the excellent https://www.chessprogramming.org/Simplified_Evaluation_Function
pieceValue :: Piece -> Int
pieceValue Pawn   = 100
pieceValue Knight = 320
pieceValue Bishop = 330
pieceValue Rook   = 500
pieceValue Queen  = 900
pieceValue King   = 20000

-- All tables taken from the excellent https://www.chessprogramming.org/Simplified_Evaluation_Function
-- NOTE: tables are mirrored with respect to website
pieceSquareTable :: Piece -> ChessData -> [Int]
pieceSquareTable Pawn _ = [0,  0,  0,  0,  0,  0,  0,  0,
                          5, 10, 10,-20,-20, 10, 10,  5,
                          5, -5,-10,  0,  0,-10, -5,  5,
                          0,  0,  0, 20, 20,  0,  0,  0,
                          5,  5, 10, 25, 25, 10,  5,  5,
                          10, 10, 20, 30, 30, 20, 10, 10,
                          50, 50, 50, 50, 50, 50, 50, 50,
                          0,  0,  0,  0,  0,  0,  0,  0]
pieceSquareTable Knight _  = [-50,-40,-30,-30,-30,-30,-40,-50,
                           -40,-20,  0,  5,  5,  0,-20,-40,
                           -30,  5, 10, 15, 15, 10,  5,-30,
                           -30,  0, 15, 20, 20, 15,  0,-30,
                           -30,  5, 15, 20, 20, 15,  5,-30,
                           -30,  0, 10, 15, 15, 10,  0,-30,
                           -40,-20,  0,  0,  0,  0,-20,-40,
                           -50,-40,-30,-30,-30,-30,-40,-50]
pieceSquareTable Rook _ = [0,  0,  0,  0,  0,  0,  0,  0,
                         5, 10, 10, 10, 10, 10, 10,  5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                        -5,  0,  0,  0,  0,  0,  0, -5,
                         0,  0,  0,  5,  5,  0,  0,  0]
pieceSquareTable Bishop _ = [-20,-10,-10,-10,-10,-10,-10,-20,
                           -10,  5,  0,  0,  0,  0,  5,-10,
                           -10, 10, 10, 10, 10, 10, 10,-10,
                           -10,  0, 10, 10, 10, 10,  0,-10,
                           -10,  5,  5, 10, 10,  5,  5,-10,
                           -10,  0,  5, 10, 10,  5,  0,-10,
                           -10,  0,  0,  0,  0,  0,  0,-10,
                           -20,-10,-10,-10,-10,-10,-10,-20]
pieceSquareTable Queen _ = [-20,-10,-10, -5, -5,-10,-10,-20,
                          -10,  0,  0,  0,  0,  0,  0,-10,
                          -10,  0,  5,  5,  5,  5,  0,-10,
                            0,  0,  5,  5,  5,  5,  0, -5,
                           -5,  0,  5,  5,  5,  5,  0, -5,
                          -10,  0,  5,  5,  5,  5,  5,-10,
                          -10,  0,  0,  0,  0,  5,  0,-10,
                          -20,-10,-10, -5, -5,-10,-10,-20]
pieceSquareTable King cd
          -- table for endgame
          | endgame cd = [ 20, 30, 10,  0,  0, 10, 30, 20,
                          20, 20,  0,  0,  0,  0, 20, 20,
                         -10,-20,-20,-20,-20,-20,-20,-10,
                         -20,-30,-30,-40,-40,-30,-30,-20,
                         -30,-40,-40,-50,-50,-40,-40,-30,
                         -30,-40,-40,-50,-50,-40,-40,-30,
                         -30,-40,-40,-50,-50,-40,-40,-30,
                         -30,-40,-40,-50,-50,-40,-40,-30]
          -- table for middle game
          | otherwise = [-50,-30,-30,-30,-30,-30,-30,-50
                        -30,-30,  0,  0,  0,  0,-30,-30,
                        -30,-10, 20, 30, 30, 20,-10,-30,
                        -30,-10, 30, 40, 40, 30,-10,-30,
                        -30,-10, 30, 40, 40, 30,-10,-30,
                        -30,-10, 20, 30, 30, 20,-10,-30,
                        -30,-20,-10,  0,  0,-10,-20,-30,
                        -50,-40,-30,-20,-20,-30,-40,-50]

endgame :: ChessData -> Bool
endgame cd = ((Queen, Black) `elem` (cd^.offPieces) && (Queen, White) `elem` (cd^.offPieces))
