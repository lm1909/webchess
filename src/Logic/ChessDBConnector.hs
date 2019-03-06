{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.ChessDBConnector where

import Logic.ChessData
import           qualified Data.Text as DT
import Logic.ChessOutput
import Database.Persist.TH

-- make custom Entities usable in Persist
derivePersistField "GameStatus"
derivePersistField "Color"


historyToText :: [Move] -> DT.Text
historyToText [] = ""
historyToText ((Move n (ox, oy) (dx, dy)):ms) = showt n <> "-" <>showt ox <> "-" <> showt oy <> "-" <> showt dx <> "-" <> showt dy <> "|" <> historyToText ms
            where showt = DT.pack . show

-- @TODO make this mess nicer
textToHistory :: DT.Text -> [Move]
textToHistory "" = []
textToHistory t = concat $ fmap move (DT.splitOn "|" t)
    where move t = wordToMove $ fmap (DT.unpack) (DT.splitOn "-" t)
          wordToMove :: [String] -> [Move]
          wordToMove (n:ox:oy:dx:dy:[]) = [Move (read n) ((read ox), (read oy)) ((read dx), (read dy))]
          wordToMove _ = []
