module Logic.OpeningBook(openingBookDepth, Variant, OpeningBook, getOpening) where

import Logic.ChessData

import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree


-- | this is a constant that determines the approximate maximum length of the opening, (= the maximum length it makes sense to search the book)
openingBookDepth :: Int
openingBookDepth = 3

-- | A variant describes one well known Form of opening a chess game
data Variant = Variant {name :: String} deriving (Show, Read)

-- | a opening book is a DAG with variants as nodes and moves as edges
type OpeningBook = Gr Variant Move

-- | returns all opening moves from the opening book that follow the given move sequence
-- NOTE: [Move] must have newest move at the head of the list
getOpening :: OpeningBook -> [Move] -> [Move]
getOpening book ms = map snd $ getOpening' book (reverse ms) 1 -- 1 is root node by convention
    where getOpening' :: OpeningBook -> [Move] -> Node -> [(Node, Move)]
          getOpening' graph [] n = lsuc graph n
          getOpening' graph (m:ms) n = concat $ map (getOpening' graph ms) $ map fst $ filter (\lm -> m == snd lm) (lsuc graph n)

--------------------------------------------------------
-- Testing related
--------------------------------------------------------

root = (1, Variant "Root")
kingpawn = (2, Variant "King Pawn's opening")

openingbook :: OpeningBook
openingbook = mkGraph [root, kingpawn] [(1, 2, Move 1 (5, 2) (5, 3))]

parsetest = showsPrec 2 openingbook

