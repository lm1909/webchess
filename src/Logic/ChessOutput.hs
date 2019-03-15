{-# LANGUAGE FlexibleInstances #-}

module Logic.ChessOutput where
    
import           Control.Lens
import           Data.Char
import           Data.Array

import           Logic.ChessData
import           Logic.ChessLegal

--------------------------------------------------------
-- * Pretty printing
-- Module adds pretty printing for everythin in Logic
--------------------------------------------------------

class Display a where
    display :: a -> String

instance Display Piece where
    display Pawn   = "P"
    display Queen  = "Q"
    display King   = "K"
    display Rook   = "R"
    display Bishop = "B"
    display Knight = "K"

instance Display Square where
    display None          = "."
    display (Ent Black p) = fmap toLower $ display p
    display (Ent White p) = fmap toUpper $ display p

instance Display Move where
    display mv = "Move #" ++ (show $ mv^.number) ++ ": " ++ (show $ mv^.orig) ++ ":" ++ (show $ mv^.dest)

instance Display (Array (Int, Int) Square) where
    display arr = unlines [ concat [display (arr ! (x, y)) | x <- [1..8]] | y <- [1..8]]

instance Display Color where
    display White = "White"
    display Black = "Black"

instance Display GameStatus where
    display = show

instance Display ChessData where
    display cd = "Game " ++ (display $ cd^.status) ++ ", on turn: " ++ (display $ cd^.playerOnTurn) ++ "\n" ++ (display $ cd^.board) ++ (concat $ fmap (\m -> ((display m) ++ "\n")) $ cd^.history) ++ (concat $ fmap (\(pc, col) -> (display col) ++ ": " ++ (display pc) ++ "\n") $ cd^.offPieces)

instance Display Reason where
    display Bounds = "Move is not in bounds of the playing field"
    display Player = "Wrong player"
    display NoPiece = "There is no piece that can be moved"
    display TakeOwn = "Not allowed to take own piece"
    display NoMove = "Must make a move"
    display QueenMove = "Illegal move for a queen"
    display KingMove = "Illegal move for a king"
    display RookMove = "Illegal move for a rook"
    display BishopMove = "Illegal move for a bishop"
    display PawnMove = "Illegal move for a pawn"
    display KnightMove = "Illegal move for a knight"
    display KingDanger = "Illegal; must not put the king in danger"
    display GameOver = "Illegal: Game is already over"


