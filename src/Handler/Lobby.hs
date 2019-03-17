{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.Lobby where

import Import
import qualified Data.Set as DS
import qualified Control.Monad.STM as STM

import           Logic.ChessData
import           Logic.ChessDBConnector

getLobbyR :: Handler Html
getLobbyR = do mmsg <- getMessage
               tv <- fmap lobby getYesod
               lobbyset <- liftIO $ readTVarIO tv
               let lobbylist = DS.elems lobbyset
               users <- runDB $ selectList [UserId <-. lobbylist] [] -- query only once
               defaultLayout $ do setTitle "Lobby" 
                                  $(widgetFile "lobby")

postJoinLobbyR :: Handler Html
postJoinLobbyR = do (authid, _) <- requireAuthPair
                    lobbytv <- fmap lobby getYesod
                    _ <- liftIO $ STM.atomically $ modifyTVar lobbytv (DS.insert authid)
                    setMessage $ toHtml ("You entered the lobby" :: Text)
                    redirect LobbyR

postLobbyPairUpR :: UserId -> Handler Html
postLobbyPairUpR pairUpOpponentId = do
    (authid, _) <- requireAuthPair

    lobbytv <- fmap lobby getYesod
    _ <- liftIO $ STM.atomically $ do modifyTVar lobbytv (DS.delete authid)
                                      modifyTVar lobbytv (DS.delete pairUpOpponentId)

    gameid <- runDB $ insert Game {gamePlayer = authid,
                                   gameOpponent = pairUpOpponentId,
                                   gameElocalcoutstanding = True,
                                   gameGameStatus = Running,
                                   gameHistory = historyToText [] }

    setMessage $ toHtml ("Paired up in lobby! Have fun!" :: Text)
    redirect (GameR gameid)
