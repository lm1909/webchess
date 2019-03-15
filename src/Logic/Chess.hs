module Logic.Chess where

import           Logic.ChessData
import           Logic.ChessLegal

--------------------------------------------------------
-- Interface
--------------------------------------------------------

makeMove :: Move -> ChessData -> Legal ChessData
makeMove mv cd = setMove mv <$> (legal mv cd)

--------------------------------------------------------
-- test / non production related
--------------------------------------------------------

testMoves1 :: [Move]
testMoves1 = [Move 1 (2, 2) (2, 3), Move 2 (4, 7) (4, 6), Move 3 (5, 2) (5, 3)]

normalPawnMove :: Move
normalPawnMove = Move 1 (2, 2) (2, 3)
