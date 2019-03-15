{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Handler.Running

Handler to show list of all running games on webchess (mainly for spectator usecase)
-}

module Handler.Running where

import           Import
import           Logic.ChessData

import           Database.Persist
import           Database.Persist.Sqlite

getRunningR :: Handler Html
getRunningR = do runninggames <- runDB $ selectList [GameGameStatus ==. Running] []
                 runningaigames <- runDB $ selectList [AiGameGameStatus ==. Running] []
                 defaultLayout $ do setTitle "Running games"
                                    $(widgetFile "running")
showGame :: Entity Game -> String
showGame (Entity key game) = "Game #" ++ (show $ unSqlBackendKey $ unGameKey key) ++ " with Status " ++ (show $ gameGameStatus game)

keyIdGame :: Entity Game -> GameId
keyIdGame (Entity key _) = key

showAiGame :: Entity AiGame -> String
showAiGame (Entity key aigame) = "AI Game #" ++ (show $ unSqlBackendKey $ unAiGameKey key) ++ " with Status " ++ (show $ aiGameGameStatus aigame)

keyIdAiGame :: Entity AiGame -> AiGameId
keyIdAiGame (Entity key _) = key
