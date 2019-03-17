{-|
Module      : Logic.OpeningBook

This module allows to read in the data of an opening book and search for moves with a certain history
-}
module Logic.OpeningBook(openingBookDepth, OpeningBook, getOpening, openingbook, parseOpeningBook, writeOut) where

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
-- Parsing
--------------------------------------------------------

-- | Read a opening book from the specified path
-- NOTE: if the specified opening book file cannot be parsed, the server doesn't even start up
parseOpeningBook :: String -> IO OpeningBook
parseOpeningBook path = do text <- readFile path
                           let ob = readsPrec 10 text
                           case length ob of 
                            1 -> return (fst $ head ob) 
                            _ -> error "Invalid opening book file"

--------------------------------------------------------
-- Testing related
--------------------------------------------------------

root = (1, Variant "Root")
kingpawn = (2, Variant "King Pawn's opening")
sicilian = (3, Variant "Sicilian Defense")
queenop = (4, Variant "Queen's opening")

openingbook :: OpeningBook
openingbook = mkGraph [root, kingpawn, sicilian, queenop] [(1, 2, Move 1 (5, 2) (5, 4)), (2, 3, Move 2 (3, 7) (3, 5)), (1, 4, Move 1 (4, 2) (4, 4))]

parsetest :: String
parsetest = showsPrec 2 openingbook ""

writeOut :: IO ()
writeOut = writeFile "./openingBook/book" parsetest

