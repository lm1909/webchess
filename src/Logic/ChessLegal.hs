{-# LANGUAGE TemplateHaskell #-}

module Logic.ChessLegal where

import           Control.Lens
import           Control.Monad
import           Data.Array
import           Logic.ChessData


data Legal x = Valid x | Invalid Reason deriving Show
data Reason = Bounds | Player | NoPiece | TakeOwn | NoMove | QueenMove | KingMove | RookMove | BishopMove | PawnMove | KnightMove | KingDanger | GameOver deriving Show

instance Functor Legal where
  fmap f (Valid cd)  = Valid $ f cd
  fmap _ (Invalid r) = Invalid r
instance Applicative Legal where
  pure  = return
  (<*>) = ap
instance Monad Legal where
  return = Valid
  (>>=) (Valid x) f   = f x
  (>>=) (Invalid r) _ = Invalid r

legal :: Move -> ChessData -> Legal ChessData -- @TODO add checkmate
legal mv cd = (legalBounds mv cd) >>= (legalPlayer mv) >>= (legalNoPiece mv) >>= (legalTakeOwn mv) >>= (legalMove mv) >>= (legalKingDanger mv) >>= (legalGameOver mv)

-- this Rulesset without the check control is needed for the legalKingDanger Rule (otherwise a cycle would ensue)
legalCheckmate :: Move -> ChessData -> Legal ChessData
legalCheckmate mv cd = (legalBounds mv cd) >>= (legalPlayer mv) >>= (legalNoPiece mv) >>= (legalTakeOwn mv) >>= (legalMove mv)

-- helper function, wraps legal to Bool
legalWrapper :: (Move -> ChessData -> Legal ChessData) -> Move -> ChessData -> Bool
legalWrapper lf mv cd = case (lf mv cd) of
                            Valid _   -> True
                            Invalid _ -> False

legalPlayer :: Move -> ChessData -> Legal ChessData
legalPlayer (Move _ o _) cd = case ((cd^.board) ! o) of
                               None -> return cd
                               (Ent col _) -> if (col == cd^.playerOnTurn) then return cd else Invalid Player

legalBounds :: Move -> ChessData -> Legal ChessData
legalBounds (Move _ o d) cd = if (primitiveInBounds o cd && primitiveInBounds d cd) then Valid cd else Invalid Bounds

primitiveInBounds :: (Int, Int) -> ChessData -> Bool
primitiveInBounds (ox, oy) cd = let ((minx, miny), (maxx, maxy)) = bounds $ cd^.board in (ox >= minx && oy >= miny && ox <= maxx && oy <= maxy)

legalNoPiece :: Move -> ChessData -> Legal ChessData
legalNoPiece (Move _ o _) cd = if ((cd^.board) ! o) == None then Invalid NoPiece else return cd

legalTakeOwn :: Move -> ChessData -> Legal ChessData
legalTakeOwn (Move _ _ d) cd = case ((cd^.board) ! d) of
                                None -> return cd
                                (Ent col _) -> if (col == cd^.playerOnTurn) then Invalid TakeOwn else return cd

legalNoMove :: Move -> ChessData -> Legal ChessData
legalNoMove (Move _ o d) cd = if (o == d) then Invalid NoMove else return cd

legalGameOver :: Move -> ChessData -> Legal ChessData
legalGameOver _ cd = case (cd^.status) of
                        Running -> return cd
                        _       -> Invalid GameOver

legalMove :: Move -> ChessData -> Legal ChessData
legalMove mv@(Move _ o _) cd = case ((cd^.board) ! o) of
                                None -> return cd
                                (Ent _ piece) -> case piece of
                                                     Rook -> legalRookMove mv cd
                                                     Pawn -> legalPawnMove mv cd
                                                     Queen -> legalQueenMove mv cd
                                                     King -> legalKingMove mv cd
                                                     Bishop -> legalBishopMove mv cd
                                                     Knight -> legalKnightMove mv cd

legalKingDanger :: Move -> ChessData -> Legal ChessData
legalKingDanger mv cd = if check ((updateMove mv) cd) (cd^.playerOnTurn) then Invalid KingDanger else return cd

-- @TODO: Pawn promotion, en passant capturing
legalPawnMove :: Move -> ChessData -> Legal ChessData
legalPawnMove (Move _ o@(ox, oy) d) cd = case cd^.playerOnTurn of
    White | (d == (ox, oy+2)) && (o == (ox, 2)) && ((cd^.board) ! d == None) -> return cd -- first move -> two allowed
          | (d == (ox, oy+1)) && ((cd^.board) ! d == None) -> return cd -- no capturing
          | ((cd^.board) ! d /= None) && (d `elem` [(ox+1, oy+1), (ox-1, oy+1)]) -> return cd -- capturing
          | otherwise -> Invalid PawnMove
    Black | (d == (ox, oy-2)) && (o == (ox, 7)) && ((cd^.board) ! d == None) -> return cd -- first move -> two allowed
          | (d == (ox, oy-1)) && ((cd^.board) ! d == None) -> return cd -- no capturing
          | ((cd^.board) ! d /= None) && (d `elem` [(ox+1, oy-1), (ox-1, oy-1)]) -> return cd -- capturing
          | otherwise -> Invalid PawnMove

legalKingMove :: Move -> ChessData -> Legal ChessData
legalKingMove (Move _ (ox, oy) d) cd = if (d `elem` [(ox+x, oy+y) | x <- [-1..1], y <- [-1..1]]) then return cd else Invalid KingMove

legalKnightMove :: Move -> ChessData -> Legal ChessData
legalKnightMove (Move _ (ox, oy) d) cd = if (d `elem` [(ox+x, oy+y) | x <- [-1, 1], y <- [-2, 2]] ++ [(ox+x, oy+y) | x <- [-2, 2], y <- [-1, 1]]) then return cd else Invalid KnightMove

legalBishopMove :: Move -> ChessData -> Legal ChessData
legalBishopMove (Move _ o d) cd = if d `elem` explore o bishopOneWayExploreFunctions cd then return cd else Invalid BishopMove

-- assumes that origin of Move is rook
legalRookMove :: Move -> ChessData -> Legal ChessData
legalRookMove (Move _ o d) cd = if (d `elem` explore o rookOneWayExploreFunctions cd) then return cd else Invalid RookMove

legalQueenMove :: Move -> ChessData -> Legal ChessData
legalQueenMove (Move _ o d) cd = if (d `elem` explore o queenOneWayExploreFunctions cd) then return cd else Invalid QueenMove

rookOneWayExploreFunctions :: [((Int, Int) -> (Int, Int))]
rookOneWayExploreFunctions = [(\(ox, oy) -> (ox+1, oy)), (\(ox, oy) -> (ox-1, oy)), (\(ox, oy) -> (ox, oy+1)), (\(ox, oy) -> (ox, oy-1))]

bishopOneWayExploreFunctions :: [((Int, Int) -> (Int, Int))]
bishopOneWayExploreFunctions = [(\(ox, oy) -> (ox+1, oy+1)), (\(ox, oy) -> (ox-1, oy-1)), (\(ox, oy) -> (ox-1, oy+1)), (\(ox, oy) -> (ox+1, oy-1))]

queenOneWayExploreFunctions :: [((Int, Int) -> (Int, Int))]
queenOneWayExploreFunctions = rookOneWayExploreFunctions ++ bishopOneWayExploreFunctions

explore :: (Int, Int) -> [((Int, Int) -> (Int, Int))] -> ChessData -> [(Int, Int)]
explore o fs cd = concat $ map (\oneWExplFun -> oneWayExplore o oneWExplFun cd) fs

-- NOTE: this function includes pieces of ones own!
oneWayExplore :: (Int, Int) -> ((Int, Int) -> (Int, Int)) -> ChessData -> [(Int, Int)]
oneWayExplore o f cd
    | not (primitiveInBounds (f o) cd) = []
    | (primitiveInBounds (f o) cd) && not ((cd^.board) ! (f o) == None) = [f o]
    | otherwise = (f o):(oneWayExplore (f o) f cd)

-- determines if the player with speciefied color is in check
check :: ChessData -> Color -> Bool
check cd col = or $ fmap check_bool $ (fmap (\p -> Move 0 p king) (getAllPositions cd' (switchColor col)))
    where king = getKingPosition cd col
          check_bool = \m -> legalWrapper legalCheckmate m cd'
          cd' = set playerOnTurn (switchColor col) cd

allMovesFromPos :: (Int, Int) -> ChessData -> [Move]
allMovesFromPos o cd = filter (\m -> legalWrapper legal m cd) $ [Move (-1) o (dx, dy) | dx <- [1..8], dy <- [1..8]]

allMovesForPlayer :: Color -> ChessData -> [Move]
allMovesForPlayer col cd = concat $ [allMovesFromPos p cd' | p <- getAllPositions cd' col]
    where cd' = set playerOnTurn (col) cd

checkMate :: Color -> ChessData -> Bool
checkMate col cd = check cd col && ((length $ allMovesForPlayer col cd) == 0)

updateGameStatus :: ChessData -> ChessData
updateGameStatus cd = if checkMate (cd^.playerOnTurn) cd then set status (Finished (Winner (switchColor (cd^.playerOnTurn)))) cd else set status (Running) cd

setMove :: Move -> ChessData -> ChessData
setMove mv cd = updateGameStatus <$> addMoveToHistory mv $ updateMove mv $ updateOffPieces mv $ updatePlayerOnTurn cd

-- Warning: does not check if moves are legal
gameFromMoves :: [Move] -> ChessData
gameFromMoves []       = newGame
gameFromMoves (mv:mvs) = setMove mv (gameFromMoves mvs)
