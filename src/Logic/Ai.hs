module Logic.Ai where

import Logic.ChessData
import Logic.ChessLegal

import Data.Array
import Control.Lens
import Data.Foldable
import Data.Tree

import Data.STRef
import Control.Monad.ST
import Control.Monad.State.Lazy

data AIDiff = Random | Easy deriving (Show, Read, Eq, Enum, Bounded)

-- this is a stable function
bestMove :: AIDiff -> ChessData -> Move
bestMove Easy cd = fst $ maximumBy maxtuple (minmaxRankings cd)
bestMove Random cd = undefined -- @TODO




 
testDraw = drawTree $ fmap (show . gameEvaluate) ((recdepth 2) (gameTree newGame))

alphabetaMax :: Int -> Int -> Int -> ChessData -> Int
alphabetaMax _ _ 0 cd = gameEvaluate cd
alphabetaMax a b n cd = runST $ do n <- newSTRef 0
                                   readSTRef n
                        

gameTree :: ChessData -> Tree ChessData
gameTree cd = Node cd (fmap gameTree (allStates cd))
    where allStates cd = fmap (\m -> setMove m cd) (allMovesForPlayer (cd^.playerOnTurn) cd)

recdepth :: Int -> Tree a -> Tree a
recdepth 0 (Node l _) = Node l []
recdepth n (Node l children) = Node l (fmap (recdepth (n-1)) children)

-- mmMax :: Tree Int -> Int
-- mmMax (Node v []) = v
-- mmMax (Node v children) = maximum (fmap mmMin children)

-- mmMin :: Tree Int -> Int
-- mmMin (Node v []) = v
-- mmMin (Node v children) = minimum (fmap mmMax children)

mmMax :: Tree Int -> Int
mmMax = maximum . mmMax'
mmMin :: Tree Int -> Int
mmMin = minimum . mmMin'

mapmin :: [[Int]] -> [Int]
mapmin (l:ls) = (minimum l) : (omit (minimum l) ls)
    where omit :: Int -> [[Int]] -> [Int]
          omit _ [] = []
          omit b (x:xs)
                | minleq b x = omit b xs
                | otherwise = ((minimum x) : omit (minimum x) xs)
mapmin [] = undefined -- not possible, only called when children != []

mmMax' :: Tree Int -> [Int]
mmMax' (Node l []) = [l]
mmMax' (Node l children) = mapmin (fmap mmMin' children)

minleq :: Int -> [Int] -> Bool
minleq _ [] = False
minleq b (x:xs) = (x <= b) || (minleq b xs)


mapmax :: [[Int]] -> [Int]
mapmax (l:ls) = (maximum l) : (omit (maximum l) ls)
        where omit :: Int -> [[Int]] -> [Int]
              omit _ [] = []
              omit b (l:ls)
                | maxgeq b l = omit b ls
                | otherwise = (maximum l) : (omit (maximum l) ls) 
mapmax [] = undefined -- not possible, only called when children != []

-- ^ sees if the maximum of the given list is greater or equal the given bound
maxgeq :: Int -> [Int] -> Bool
maxgeq _ [] = False
maxgeq b (x:xs) = (x >= b) || (maxgeq b xs)



mmMin' :: Tree Int -> [Int]
mmMin' (Node v []) = [v]
mmMin' (Node v children) = fmap maximum (fmap mmMax' children)


-- sumST :: Num a => [a] -> a
-- sumST xs = runST $ do           -- runST takes out stateful code and makes it pure again.

--     n <- newSTRef 0             -- Create an STRef (place in memory to store values)

--     forM_ xs $ \x -> do         -- For each element of xs ..
--         modifySTRef n (+x)      -- add it to what we have in n.

--     readSTRef n                 -- read the value of n, and return it.



optimisationDirection :: Color -> Int
optimisationDirection Black = -1
optimisationDirection White = 1

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
