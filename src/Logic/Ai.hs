module Logic.Ai where

import           Logic.ChessData
import           Logic.ChessLegal
import           Logic.OpeningBook

import           Control.Parallel.Strategies
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
bestMove :: OpeningBook -> AIDiff -> ChessData -> Move
bestMove _ Easy cd   = minmaxMax 3 (cd^.playerOnTurn) cd
bestMove ob Medium cd -- atm this always chooses the first move; deciding on for example statistical information for future feature?
    | ((length (cd^.history) < openingBookDepth) && (length (getOpening ob (cd^.history)) > 0)) = ((getOpening ob (cd^.history)) !! 0) -- move from opening book if still opening phase & move found
    | otherwise = snd $ dynprunAlphaBeta 5 (cd^.playerOnTurn) cd -- do alpha-beta-search

--------------------------------------------------------
-- General
--------------------------------------------------------

-- | gameTree generates a tree of all possible moves in a chess game origining from the provided ChessData
-- lazy evaluation is essential here, the tree structure is infinite
gameTree :: ChessData -> Tree ChessData
gameTree cd = Node cd (map gameTree (allChessData cd)) -- `using` parList rpar

-- | verticalprune limits the game tree to a finite depth
verticalprune :: Int -> Tree a -> Tree a
verticalprune 0 (Node v _) = Node v []
verticalprune n (Node v children) = Node v (fmap (verticalprune (n-1)) children)

-- | horizontalprune allows for a possible eager optimization: only consider the best moves of some children
-- NOTE: only act on ordered tree to not detriment move quality
horizontalprune :: Int -> Tree a -> Tree a
horizontalprune p (Node v children) = Node v (fmap (horizontalprune p) (take ((length children) `quot` p) children))

-- GTree: used for alpha-beta to store associated move in Tree as computation optimally is as near at the root as possible

type GTree a = Tree (a, Move)

-- | very similar to gameTree; only additionaly saves the associated move ('second level' in the complete tree) for all subsequent possible states
gamemoveTree :: ChessData -> GTree ChessData
gamemoveTree cd = Node (cd, undefined) (fmap (\(cds, m) -> gamemoveTree' cds m) (allStates cd))
    where allStates cd = fmap (\m -> (setMove m cd, m)) (allMovesForPlayer (cd^.playerOnTurn) cd)

          gamemoveTree' :: ChessData -> Move -> GTree ChessData
          gamemoveTree' cd m = Node (cd, m) (fmap (\mov -> gamemoveTree' (setMove mov cd) m) (allMovesForPlayer (cd^.playerOnTurn) cd))

-- | similar to verticalprune, additionaly supports a quieescence search by allowing for dynamical pruning
dynverticalprune :: Int -> (ChessData -> Bool) -> GTree ChessData -> GTree ChessData
dynverticalprune 0 d (Node (cd, m) children)
    | d cd = Node (cd, m) (fmap (dynverticalprune 0 d) children)
    | otherwise = Node (cd, m) []
dynverticalprune n d (Node v children) = Node v (fmap (dynverticalprune (n-1) d) children)


-- helper functions

gminimum = minimumBy (comparing fst)
gmaximum = maximumBy (comparing fst)

gfmap :: (a -> b) -> (GTree a) -> (GTree b)
gfmap f (Node (a, m) children) = Node (f a, m) (fmap (gfmap f) children)

--------------------------------------------------------
-- * Parallel tree based MinMax
-- implements a simple min-max search (https://www.chessprogramming.org/Minimax)
-- uses the parallel library for multi-core computation
--------------------------------------------------------

-- | min-max-search optimal move accessor function
-- NOTE: depth is one bigger than provided, as minmaxMax calculates mmSearch for all possible moves
minmaxMax :: Int -> Color -> ChessData -> Move
minmaxMax d col cd = snd $ maximumBy (comparing fst) $ (fmap (\m -> (mmSearch d col (setMove m cd), m)) (allMovesForPlayer (cd^.playerOnTurn) cd) ) -- `using` parList rdeepseq

-- | min-max-search (see: https://www.chessprogramming.org/Minimax)
mmSearch :: Int -> Color -> ChessData -> Int
mmSearch d col = maximum . ((\(Node _ children) -> map mmMin children `using` parList rdeepseq)) . (\t -> (fmap (\cd -> (optimisationDirection col) * (gameEvaluate cd) ) t) ) . verticalprune d . gameTree -- `using` parTraversable rpar


-- pmap :: (a -> b) -> Tree a -> Tree b
-- pmap f (Node a children)

-- parmap :: (a -> b) -> (Tree a) -> (Tree b) parmap f (Node v []) = Node (f v) []
-- parmap f (Node v children) = runEval $ do children' <- parmap 
--                                           v' <- f v `using` rseq

-- treeStrat :: (NFData a) => Int -> Strategy (Tree a)
-- treeStrat _ (Node v []) = do v' <- rdeepseq v
--                              return (Node v' [])
-- treeStrat 0 (Node v children) = do children' <- rdeepseq children
--                                    v' <- rdeepseq v
--                                    return (Node v' children')                                
-- treeStrat d (Node v children) = do children' <- (parList $ treeStrat (d-1)) children
--                                    v' <- rseq v
--                                    return (Node v' children')                                

-- treeStrat :: (NFData a) => Strategy (Tree a)
-- treeStrat (Node a children) = do a' <- a `using` (rparWith rdeepseq)
--                                  return (Node a' children)

mmMax :: Tree Int -> Int
mmMax (Node v []) = v
mmMax (Node v children) = maximum (map mmMin children)

mmMin :: Tree Int -> Int
mmMin (Node v []) = v
mmMin (Node v children) = minimum (fmap mmMax children)

--------------------------------------------------------
-- Optimized alpha-beta-Search
-- implements an optimized alpha-beta search (https://www.chessprogramming.org/Alpha-Beta)
-- with quiescence search like vertical pruning, move ordering and horizontal pruning
--------------------------------------------------------

-- accessor function for a alpha-beta-pruning move optimizing search (https://www.chessprogramming.org/Alpha-Beta), includes quiescence, move odering, horizontal pruning
dynprunAlphaBeta :: Int -> Color -> ChessData -> (Int, Move)
dynprunAlphaBeta d col = (gmaximum . abMax . horizontalprune 2 . orderhigher . gfmap (\cd -> (optimisationDirection col) * (gameEvaluate cd)) . dynverticalprune d quiescence . gamemoveTree)


abMax :: GTree Int -> [(Int, Move)]
abMax (Node v []) = [v]
abMax (Node _ children) = mapmin (fmap abMin children)
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

abMin :: GTree Int -> [(Int, Move)]
abMin (Node v []) = [v]
abMin (Node _ children) = mapmax (fmap abMax children)
    where mapmax :: [[(Int, Move)]] -> [(Int, Move)]
          mapmax (l:ls) = (gmaximum l) : (betacut (fst $ gmaximum l) ls)
          -- note mapmax [] is not possible, as it is never called as that

          betacut :: Int -> [[(Int, Move)]] -> [(Int, Move)]
          betacut _ [] = []
          betacut beta (l:ls)
            | maxgeq beta l = betacut beta ls
            | otherwise = (gmaximum l) : (betacut (fst $ gmaximum l) ls)

          -- | sees if the maximum of the given list is greater or equal the given bound
          maxgeq :: Int -> [(Int, Move)] -> Bool
          maxgeq _ []        = False
          maxgeq beta ((x, _):xs) = (x >= beta) || (maxgeq beta xs)


-- @TODO implement a quiescence search (this is quite a lot of work)
-- see https://www.chessprogramming.org/CPW-Engine_quiescence
quiescence :: ChessData -> Bool
quiescence _ = False -- @TODO

-- move ordering to increase efficiency of alpha-/beta- cuts
orderhigher :: GTree Int -> GTree Int
orderhigher (Node v children) = Node v (sortBy (flip (comparing (\(Node (i, _) _) -> i))) (map orderlower children))
orderlower :: GTree Int -> GTree Int
orderlower (Node v children) = Node v (sortBy (comparing (\(Node (i, _) _) -> i)) (map orderhigher children))

--------------------------------------------------------
-- Naive MiniMax implemenation
--------------------------------------------------------

-- maxtuple :: (a, Int) -> (a, Int) -> Ordering
-- maxtuple (_, i) (_, j) = compare i j

-- minmaxRankings :: ChessData -> [(Move, Int)]
-- minmaxRankings cd =  (fmap (\m -> (m, (minmax' 2 (cd^.playerOnTurn)) $ setMove m cd)) (allMovesForPlayer (_playerOnTurn cd) cd))


-- movePair :: ChessData -> (Move, Int)
-- movePair cd = maximumBy maxtuple (minmaxRankings cd)

-- minmax' :: Int -- ^ search depth
--             -> Color -- ^ color of the player which is trying to optimize his move
--             -> ChessData -- ^ chess situation
--             -> Int -- ^ index for how good the situation is
-- minmax' 0 maxplayer cd = (optimisationDirection maxplayer) * (gameEvaluate cd)
-- minmax' n maxplayer cd = case (allMovesForPlayer (cd^.playerOnTurn) cd) of
--                             [] -> (optimisationDirection maxplayer) * (optimisationDirection (cd^.playerOnTurn)) * (-1000000000)
--                             mvs -> (optifun (cd^.playerOnTurn)) $ fmap (\m -> minmax' (n-1) maxplayer (setMove m cd)) mvs
--     where optifun c = if c==maxplayer then maximum else minimum




--------------------------------------------------------
-- * Instantanious Evaluation functions
--------------------------------------------------------

-- | returns optimisation direction for the different player colors
optimisationDirection :: Color -> Int
optimisationDirection Black = -1
optimisationDirection White = 1

-- | instantanious board evaluation function (piece value & piece-square table analysis) for the player on turn
-- positive is good for white; the bigger the better; score is symmetric for black
gameEvaluate :: ChessData -> Int
gameEvaluate cd = sum [ evalSquare ((cd^.board) ! (x, y)) (x, y) | x <- [1..8], y <- [1..8]]
    where evalSquare None _ = 0
          evalSquare (Ent col pc) p = (acc col p) (pieceSquareTable pc cd) + (if (col==White) then 1 else -1)*(pieceValue pc)
            where acc White (x, y) = (\a -> a !! ((x-1)+((y-1)*8)))
                  acc Black (x, y) = (\a -> -1*(a !! (((9-x)-1)+(((9-y)-1)*8)))   )

-- | gives value of a piece in centipawns
-- taken from the excellent https://www.chessprogramming.org/Simplified_Evaluation_Function
pieceValue :: Piece -> Int
pieceValue Pawn   = 100
pieceValue Knight = 320
pieceValue Bishop = 330
pieceValue Rook   = 500
pieceValue Queen  = 900
pieceValue King   = 20000

-- | piece-square lookup tables for instatanous board eval; respects game phase
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

-- | determines if game is in endgame phase
endgame :: ChessData -> Bool
endgame cd = ((Queen, Black) `elem` (cd^.offPieces) && (Queen, White) `elem` (cd^.offPieces))
