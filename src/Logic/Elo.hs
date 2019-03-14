module Logic.Elo where

import Logic.ChessData

-- | returns the updated elos after a game (according to FIDE rules)
-- see: https://quantdare.com/elo-system/
eloUpdate :: Result -> Double -> Double -> (Double, Double) 
eloUpdate result eloWhite eloBlack = (eloWhite + 40.0 * ((rating result White) - (eloExpectation eloWhite eloBlack)), eloBlack + 40.0 * ((rating result Black) - (eloExpectation eloBlack eloWhite)))
    where rating :: Result -> Color -> Double
          rating Draw _ = 0.5
          rating (Winner win) player = if (win == player) then 1.0 else (-1.0)

-- | computes the expected win probability for the first player 
eloExpectation :: Double -> Double -> Double
eloExpectation eloA eloB = 1.0 / (1 + 10.0**( (eloB - eloA) / 400.0))
