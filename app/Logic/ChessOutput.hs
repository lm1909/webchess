{-# LANGUAGE TemplateHaskell #-}

module Logic.ChessOutput where
    
import Logic.ChessData
import           Data.Array
import Control.Lens
import           Data.Char
import Prelude

instance Show Piece where
    show Pawn   = "P"
    show Queen  = "Q"
    show King   = "K"
    show Rook   = "R"
    show Bishop = "B"
    show Knight = "K"

instance Show Square where
  show None          = "."
  show (Ent Black p) = fmap toLower $ show p
  show (Ent White p) = fmap toUpper $ show p

instance Show Move where
    show mv = "Move #" ++ (show $ mv^.number) ++ ": " ++ (show $ mv^.orig) ++ ":" ++ (show $ mv^.dest)

instance Show ChessData where
    show cd = "Game " ++ (show $ cd^.status) ++ ", on turn: " ++ (show $ cd^.playerOnTurn)
              ++ "\n" ++ (showBoard $ cd^.board) ++ (concat $ fmap (\m -> (show m) ++ "\n") $ cd^.history)

showBoard :: Board -> String
showBoard arr = unlines [ concat [show (arr ! (x, y)) | x <- [1..8]] | y <- [1..8]]


