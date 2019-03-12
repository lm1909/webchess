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

data AIDiff = Easy | Medium deriving (Show, Read, Eq, Enum, Bounded)

-- this is a stable function
bestMove :: AIDiff -> ChessData -> Move
bestMove Easy cd   = fst $ maximumBy maxtuple (minmaxRankings cd)
bestMove Medium cd = snd $ dynprunAlphaBeta 6 (cd^.playerOnTurn) cd

--------------------------------------------------------
-- General
--------------------------------------------------------

type GTree a = Tree (a, Move)

gameTree :: ChessData -> GTree ChessData
gameTree cd = Node (cd, undefined) (fmap (\(cds, m) -> gameTree' cds m) (allStates cd))

gameTree' :: ChessData -> Move -> GTree ChessData
gameTree' cd m = Node (cd, m) (fmap (\mov -> gameTree' (setMove mov cd) m) (allMovesForPlayer (cd^.playerOnTurn) cd))

allStates :: ChessData -> [(ChessData, Move)]
allStates cd = fmap (\m -> (setMove m cd, m)) (allMovesForPlayer (cd^.playerOnTurn) cd)

recdepth :: Int -> (ChessData -> Bool) -> GTree ChessData -> GTree ChessData
recdepth 0 d (Node (cd, m) children)
    | d cd = Node (cd, m) (fmap (recdepth 0 d) children)
    | otherwise = Node (cd, m) []
recdepth n d (Node v children) = Node v (fmap (recdepth (n-1) d) children)

--------------------------------------------------------
-- Optimized alpha-beta-Search
--------------------------------------------------------


dynprunAlphaBeta :: Int -> Color -> ChessData -> (Int, Move)
dynprunAlphaBeta d col = (gmaximum . mmMax' . horizontalprune 2 . orderhigher . gfmap (\cd -> (optimisationDirection col) * (gameEvaluate cd)) . recdepth d quiescence . gameTree)

-- NOTE: only act on ordered tree to not detriment move quality
horizontalprune :: Int -> GTree a -> GTree a
horizontalprune p (Node v children) = Node v (fmap (horizontalprune p) (take ((length children) `quot` p) children))

-- @TODO implement a quiescence search (this is quite a lot of work)
-- see https://www.chessprogramming.org/CPW-Engine_quiescence
quiescence :: ChessData -> Bool
quiescence _ = False -- @TODO

alphabetaMax :: GTree Int -> (Int, Move)
alphabetaMax = gmaximum . mmMax'
alphabetaMin :: GTree Int -> (Int, Move)
alphabetaMin = gminimum . mmMin'

gminimum = minimumBy (comparing fst)
gmaximum = maximumBy (comparing fst)
gfmap :: (a -> b) -> (GTree a) -> (GTree b)
gfmap f (Node (a, m) children) = Node (f a, m) (fmap (gfmap f) children)

mmMax' :: GTree Int -> [(Int, Move)]
mmMax' (Node v []) = [v]
mmMax' (Node _ children) = mapmin (fmap mmMin' children)
    where mapmin :: [[(Int, Move)]] -> [(Int, Move)]
          mapmin (l:ls) = (gminimum l) : (alphacut (fst $ gminimum l) ls)
          -- note mapmin [] is not possible, as it is never called as that

          alphacut :: Int -> [[(Int, Move)]] -> [(Int, Move)]
          alphacut _ [] = []
          alphacut alpha (x:xs)
                | minleq alpha x = alphacut alpha xs
                | otherwise = ((gminimum x) : alphacut (fst $ gminimum x) xs)

          minleq :: Int -> [(Int, Move)] -> Bool
          minleq _ []         = False
          minleq alpha ((x, _):xs) = (x <= alpha) || (minleq alpha xs)

mmMin' :: GTree Int -> [(Int, Move)]
mmMin' (Node v []) = [v]
mmMin' (Node _ children) = mapmax (fmap mmMax' children)
    where mapmax :: [[(Int, Move)]] -> [(Int, Move)]
          mapmax (l:ls) = (gmaximum l) : (betacut (fst $ gmaximum l) ls)
          -- note mapmax [] is not possible, as it is never called as that

          betacut :: Int -> [[(Int, Move)]] -> [(Int, Move)]
          betacut _ [] = []
          betacut beta (l:ls)
            | maxgeq beta l = betacut beta ls
            | otherwise = (gmaximum l) : (betacut (fst $ gmaximum l) ls)

          -- ^ sees if the maximum of the given list is greater or equal the given bound
          maxgeq :: Int -> [(Int, Move)] -> Bool
          maxgeq _ []        = False
          maxgeq beta ((x, _):xs) = (x >= beta) || (maxgeq beta xs)


orderhigher :: GTree Int -> GTree Int
orderhigher (Node v children) = Node v (sortBy (flip (comparing (\(Node (i, _) _) -> i))) (map orderlower children))

orderlower :: GTree Int -> GTree Int
orderlower (Node v children) = Node v (sortBy (comparing (\(Node (i, _) _) -> i)) (map orderhigher children))

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

-- mmMax :: Tree Int -> Int
-- mmMax (Node v []) = v
-- mmMax (Node v children) = maximum (fmap mmMin children)

-- mmMin :: Tree Int -> Int
-- mmMin (Node v []) = v
-- mmMin (Node v children) = minimum (fmap mmMax children)

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
