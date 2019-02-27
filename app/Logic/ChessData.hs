module Logic.ChessData where

import           Data.Array
import           Data.Char
import           Prelude
import           Data.Array.ST
import           Data.STRef
import           Control.Monad.ST

-- Immutable Data Type
-- convention for index: (x, y); (1, 1) is left rook of white
type BoardI = Array (Int, Int) Square
data ChessDataI = ChessDataI {  statusI       :: GameStatus,
                                boardI        :: BoardI,
                                playerOnTurnI :: Color,
                                offPiecesI    :: [(Piece, Color)],
                                historyI      :: [Move] --history, youngest move is first in list
                             }

-- Mutable Data Type
type BoardM s = STArray s (Int, Int) Square
data ChessDataM s = ChessDataM { status :: STRef s GameStatus,
                               board :: BoardM s,
                               playerOnTurn :: STRef s Color,
                               offPieces :: STRef s [(Piece, Color)],
                               history :: STRef s [Move]
                             }

instance Show ChessDataI where
    show cd = "Game " ++ (show $ statusI cd) ++ ", on turn: " ++ (show $ playerOnTurnI cd)
              ++ "\n" ++ (showBoard $ boardI cd) ++ (concat $ fmap (\m -> (show m) ++ "\n") $ historyI cd)

data GameStatus = Running | Finished deriving (Show, Eq)
data Color = Black | White deriving (Eq, Show)

data Piece = Hook | Pawn | Queen | King | Rook | Bishop | Knight deriving Eq
instance Show Piece where
    show Hook   = "H"
    show Pawn   = "p"
    show Queen  = "Q"
    show King   = "K"
    show Rook   = "R"
    show Bishop = "B"
    show Knight = "K"

data Square = Empty | Ent Color Piece
instance Show Square where
  show Empty         = "."
  show (Ent Black p) = fmap toLower $ show p
  show (Ent White p) = fmap toUpper $ show p

data Move = Move { number :: Int, from :: (Int, Int), to :: (Int, Int)}
instance Show Move where
    show mv = "Move #" ++ (show $ number mv) ++ ": " ++ (show $ from mv) ++ ":" ++ (show $ to mv)

showBoard :: BoardI -> String
showBoard arr = unlines [ concat [show (arr ! (x, y)) | x <- [1..8]] | y <- [1..8]]

initBoardI :: BoardI
initBoardI = array ((1, 1), (8, 8)) completeBoard
    where completeBoard = firstLine White 1
                          ++ pawnLine White 2
                          ++ (foldl1 (++) [emptyLine n | n <- [3..6]])
                          ++ pawnLine Black 7
                          ++ firstLine Black 8
pawnLine :: Color -> Int -> [((Int, Int), Square)]
pawnLine c n = [((i, n), Ent c Pawn) | i <- [1..8]]
firstLine :: Color -> Int -> [((Int, Int), Square)]
firstLine c n = [((1, n), Ent c Rook),
                 ((2, n), Ent c Knight),
                 ((3, n), Ent c Bishop),
                 ((4, n), Ent c Queen),
                 ((5, n), Ent c King),
                 ((6, n), Ent c Bishop),
                 ((7, n), Ent c Knight),
                 ((8, n), Ent c Rook)]
emptyLine :: Int -> [((Int, Int), Square)]
emptyLine n = [((i, n), Empty) | i <- [1..8]]

newGame :: ChessDataI
newGame = ChessDataI Running initBoardI White [] []

thawChessData :: ChessDataI -> ST s (ChessDataM s)
thawChessData cd = do status' <- newSTRef $ statusI cd
                      board' <- thaw $ boardI cd
                      playerOnTurn' <- newSTRef $ playerOnTurnI cd
                      offPieces' <- newSTRef $ offPiecesI cd
                      history' <- newSTRef $ historyI cd
                      return $ ChessDataM status' board' playerOnTurn' offPieces' history'         

freezeChessData :: ChessDataM s -> ST s ChessDataI
freezeChessData cd = do
    status' <- readSTRef $ status cd
    board' <- freeze $ board cd
    playerOnTurn' <- readSTRef $ playerOnTurn cd
    offPieces' <- readSTRef $ offPieces cd
    history' <- readSTRef $ history cd
    return (ChessDataI status' board' playerOnTurn' offPieces' history')





