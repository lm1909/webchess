{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Running where

import Import
import Logic.ChessData
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

getRunningR :: Handler Html
getRunningR = do runninggames <- runDB $ selectList [GameGameStatus ==. Running] []
                 defaultLayout $ do setTitle "Running games"
                                    $(widgetFile "running")
showGame :: Entity Game -> String
showGame (Entity key game) = "Game #" ++ (show $ unSqlBackendKey $ unGameKey key) ++ " with Status " ++ (show $ gameGameStatus game)
-- Game GameKey {unGameKey = SqlBackendKey {unSqlBackendKey = 3}}

-- keyIdGame :: Entity Game -> GameId
keyIdGame (Entity key _) = key
