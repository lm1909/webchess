{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.ChessDBConnector where

import Logic.ChessData
import Logic.Ai

import Database.Persist.TH
import qualified Data.Text as DT

-- make custom Entities usable in Persist
derivePersistField "GameStatus"
derivePersistField "Color"
derivePersistField "AIDiff"


historyToText :: [Move] -> DT.Text
historyToText [] = ""
historyToText ((Move n (ox, oy) (dx, dy)):ms) = showt n <> ":" <> showt ox <> ":" <> showt oy <> ":" <> showt dx <> ":" <> showt dy <> "|" <> historyToText ms
            where showt = DT.pack . show

-- @TODO make this mess nicer
textToHistory :: DT.Text -> [Move]
textToHistory "" = []
textToHistory t = concat $ fmap move (DT.splitOn "|" t)
    where move text = wordToMove $ fmap (DT.unpack) (DT.splitOn ":" text)
          wordToMove :: [String] -> [Move]
          wordToMove (n:ox:oy:dx:dy:[]) = [Move (read n) ((read ox), (read oy)) ((read dx), (read dy))]
          wordToMove _ = []
