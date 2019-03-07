{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Ranking where

import Import
import Logic.ChessData
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

getRankingR :: Handler Html
getRankingR = do besthundredplayers <- runDB $ selectList [UserElo >. 1000] [Desc UserElo]
                 defaultLayout $ do setTitle "Ranking"
                                    $(widgetFile "ranking")

showPlayer :: Entity User -> String
showPlayer (Entity key player) = (show $ userNick player) ++ " with an elo of " ++ (show $ userElo player)

keyIdPlayer (Entity key _) = key
