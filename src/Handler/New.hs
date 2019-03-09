{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.New where

import Import
import Yesod

import Logic.ChessDBConnector
import Logic.ChessData
import Logic.Ai

data HumanGameForm = HumanGameForm {opponent :: Text} deriving Show
data AiGameForm = AiGameForm {difficulty :: AIDiff} deriving Show

humanGameForm :: Form HumanGameForm
humanGameForm = renderBootstrap $ HumanGameForm
                <$> areq textField "Opponent Nickname" Nothing

aiGameForm :: Form AiGameForm
aiGameForm = renderBootstrap $ AiGameForm
                <$> areq (selectField optionsEnum) "" Nothing

getNewR :: Handler Html
getNewR = do (humanwidget, humanenctype) <- generateFormPost humanGameForm
             (aiwidget, aienctype) <- generateFormPost aiGameForm 
             defaultLayout $ do setTitle "New Game"
                                $(widgetFile "new")

postNewHumanR :: Handler Html
postNewHumanR = do ((result, humanwidget), enctype) <- runFormPost humanGameForm
                   case result of
                       FormSuccess game -> do (id, user) <- requireAuthPair
                                              opponent <- runDB $ getBy $ UniqueNick (opponent game)
                                              case opponent of 
                                                   Nothing -> do setMessage $ toHtml ("Game creation failed: No such user exists" :: Text)
                                                                 redirect NewR
                                                   (Just (Entity key val)) -> do gameid <- runDB $ insert Game {gamePlayer = id,
                                                                                                                gameOpponent = key,
                                                                                                                gameGameStatus = Running,
                                                                                                                gameHistory = historyToText [] }
                                                                                 redirect (GameR gameid) 
                       _ -> redirect (NewR)

postNewAiR :: Handler Html
postNewAiR = do ((result, aiwidget), enctype) <- runFormPost aiGameForm
                case result of
                    FormSuccess aigame -> do (id, user) <- requireAuthPair
                                             aigameid <- runDB $ insert AiGame {aiGamePlayer = id,
                                                                                aiGameDiff = difficulty aigame,
                                                                                aiGameGameStatus = Running,
                                                                                aiGameHistory = historyToText [] }
                                             redirect (AiGameR aigameid)
                    _ -> redirect NewR
