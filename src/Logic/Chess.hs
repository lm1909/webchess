module Logic.Chess where

import Logic.ChessData
import Logic.ChessOutput
import Logic.ChessLegal

testMoves1 :: [Move]
testMoves1 = [Move 1 (2, 2) (2, 3), Move 2 (4, 7) (4, 6), Move 3 (5, 2) (5, 3)]

normalPawnMove :: Move
normalPawnMove = Move 1 (2, 2) (2, 3)

makeMove :: Move -> ChessData -> Legal ChessData
makeMove mv cd = setMove mv <$> (legal mv cd)




