{-# LANGUAGE TemplateHaskell #-}

module Logic.ChessLegal where

import           Control.Lens
import           Control.Monad
import           Data.Array
import Logic.Util
import Logic.ChessData


data Legal x = Valid x | Invalid Reason deriving Show
data Reason = Bounds | Player | NoPiece | TakeOwn | NoMove | QueenMove | KingMove | RookMove | BishopMove | PawnMove | KnightMove | KingDanger deriving Show

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
legal mv cd = (legalBounds mv cd) >>= (legalPlayer mv) >>= (legalNoPiece mv) >>= (legalTakeOwn mv) >>= (legalMove mv) >>= (legalKingDanger mv)

-- this Ruleset without the check control is needed for the legalKingDanger Rule (otherwise a cycle would ensue)
legalCheckmate :: Move -> ChessData -> Legal ChessData
legalCheckmate mv cd = (legalBounds mv cd) >>= (legalPlayer mv) >>= (legalNoPiece mv) >>= (legalTakeOwn mv) >>= (legalMove mv)

-- helper function, wraps legal to Bool
legalWrapper :: (Move -> ChessData -> Legal ChessData) -> Move -> ChessData -> Bool
legalWrapper lf mv cd = case (lf mv cd) of 
                            Valid _ -> True
                            Invalid _ -> False

legalPlayer :: Move -> ChessData -> Legal ChessData
legalPlayer (Move _ o _) cd = case ((cd^.board) ! o) of
                               None -> return cd
                               (Ent col _) -> if (col == cd^.playerOnTurn) then return cd else Invalid Player

legalBounds :: Move -> ChessData -> Legal ChessData
legalBounds (Move _ o d) cd = if (d < minb || d > maxb || o < minb || o > maxb) then Invalid Bounds else Valid cd
                                where (minb, maxb) = bounds $ cd^.board

legalNoPiece :: Move -> ChessData -> Legal ChessData
legalNoPiece (Move _ o _) cd = if ((cd^.board) ! o) == None then Invalid NoPiece else return cd

legalTakeOwn :: Move -> ChessData -> Legal ChessData
legalTakeOwn (Move _ _ d) cd = case ((cd^.board) ! d) of 
                                None -> return cd
                                (Ent col _) -> if (col == cd^.playerOnTurn) then Invalid TakeOwn else return cd

legalNoMove :: Move -> ChessData -> Legal ChessData
legalNoMove (Move _ o d) cd = if (o == d) then Invalid NoMove else return cd

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
legalKingDanger mv cd = if check $ over (board) (updateMove mv) cd then Invalid KingDanger else return cd

legalPawnMove :: Move -> ChessData -> Legal ChessData
legalPawnMove (Move _ (ox, oy) d) cd
    | (d == (ox, oy+1)) && ((cd^.board) ! d == None) = return cd -- no capturing
    | ((cd^.board) ! d /= None) && (d `elem` [(ox+1, oy+1), (ox-1, oy+1)]) = return cd -- capturing
    | otherwise = Invalid PawnMove
-- @TODO: Pawn promotion, en passant capturing

legalKingMove :: Move -> ChessData -> Legal ChessData
legalKingMove (Move _ (ox, oy) d) cd = if (d `elem` [(ox+x, oy+y) | x <- [-1..1], y <- [-1..1]]) then return cd else Invalid KingMove

legalKnightMove :: Move -> ChessData -> Legal ChessData
legalKnightMove (Move _ (ox, oy) d) cd = if (d `elem` [(ox+x, oy+y) | x <- [-1, 1], y <- [-2, 2]] ++ [(ox+x, oy+y) | x <- [-2, 2], y <- [-1, 1]]) then return cd else Invalid KnightMove
                                                     
legalBishopMove :: Move -> ChessData -> Legal ChessData
legalBishopMove (Move _ (ox, oy) d@(dx, dy)) cd = if (d `elem` [(ox+n, oy+n) | n <- [-8..8]] ++ [(ox+n, oy-n) | n <- [-8..8]]) && (and $ fmap ((==) None) $ fmap ((!) (cd^.board)) inter) then return cd else Invalid BishopMove
    where inter = ([(ox+x*n, oy+y*n) | n <- [1..(abs dx-ox)]]) where x = sign dx-ox
                                                                     y = sign dy-oy

legalRookMove :: Move -> ChessData -> Legal ChessData
legalRookMove (Move _ (ox, oy) (dx, dy)) cd = if (ox == dx || oy == dy) && (and $ fmap ((==) None) $ fmap ((!) (cd^.board)) inter) then return cd else Invalid RookMove
    where inter = (\xs -> take (length xs -1) xs) $ drop 1 ([(ox+x, oy+y) | x <- [dx-ox..0], y <- [dy-dx..0]] ++ [(ox+x, oy+y) | x <- [0..dx-ox], y <- [0..dy-dx]])

legalQueenMove :: Move -> ChessData -> Legal ChessData
legalQueenMove mv cd = case (legalBishopMove mv cd)  of 
                     Valid _ -> return cd
                     Invalid _ -> case (legalRookMove mv cd) of 
                                    Valid _ -> return cd
                                    Invalid _ -> Invalid QueenMove

-- determines if the player at turn is in check
check :: ChessData -> Bool
check cd = or $ fmap check_bool $ (fmap (\p -> Move 0 p king) (getAllPositions cd (switchColor $ cd^.playerOnTurn)))
    where king = getKingPosition cd (cd^.playerOnTurn)
          check_bool = \m -> legalWrapper legalCheckmate m cd




