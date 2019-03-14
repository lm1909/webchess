{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.New where

import           Import

import           Logic.Ai
import           Logic.ChessData
import           Logic.ChessDBConnector

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
postNewHumanR = do ((result, humanwidget), _) <- runFormPost humanGameForm
                   case result of
                       FormSuccess game -> do (authid, _) <- requireAuthPair
                                              opponent <- runDB $ getBy $ UniqueNick (opponent game)
                                              case opponent of
                                                   Nothing -> do setMessage $ toHtml ("Game creation failed: No such user exists" :: Text)
                                                                 redirect NewR
                                                   (Just (Entity key _)) -> do gameid <- runDB $ insert Game {gamePlayer = authid,
                                                                                                              gameOpponent = key,
                                                                                                              gameElocalcoutstanding = True,
                                                                                                              gameGameStatus = Running,
                                                                                                              gameHistory = historyToText [] }
                                                                               redirect (GameR gameid)
                       _ -> redirect (NewR)

postNewAiR :: Handler Html
postNewAiR = do ((result, aiwidget), _) <- runFormPost aiGameForm
                case result of
                    FormSuccess aigame -> do (authid, _) <- requireAuthPair
                                             aigameid <- runDB $ insert AiGame {aiGamePlayer = authid,
                                                                                aiGameDiff = difficulty aigame,
                                                                                aiGameGameStatus = Running,
                                                                                aiGameThinking = False,
                                                                                aiGameHistory = historyToText [] }
                                             redirect (AiGameR aigameid)
                    _ -> redirect NewR
