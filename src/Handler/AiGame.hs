{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.AiGame where

import Import
import Control.Lens

import Render.HtmlRender
import Logic.Ai
import Logic.ChessData
import Logic.ChessLegal
import Logic.ChessDBConnector
import Logic.Chess


aiGameToChessData :: AiGame -> ChessData
aiGameToChessData aigame = gameFromMoves (textToHistory $ aiGameHistory aigame)

getAiGameR :: AiGameId -> Handler Html
getAiGameR aiGameId = do aigame <- runDB $ get404 aiGameId
                         (id, user) <- requireAuthPair
                         ((res, movewidget), enctype) <- runFormGet moveForm
                         let cd = aiGameToChessData aigame
                         player <- runDB $ get404 (aiGamePlayer aigame) -- @TODO is a 404 really optimal here?
                         defaultLayout $ do setTitle "AI Match"
                                            addScriptRemote "http://code.jquery.com/jquery-latest.js" -- this is necessary for the live update view js
                                            $(widgetFile "aigame")

postAiGameR :: AiGameId -> Handler Html
postAiGameR aiGameId = do ((result, widget), enctype) <- runFormPostNoToken moveForm -- @TODO enable cross site request forgery protection
                          aigame <- runDB $ get404 aiGameId
                          (id, user) <- requireAuthPair
                          player <- runDB $ get404 (aiGamePlayer aigame) -- @TODO is a 404 really optimal here?
                          let cd = aiGameToChessData aigame
                          case (((id == aiGamePlayer aigame) && (cd^.playerOnTurn == White))) of
                            True -> case result of
                                            FormSuccess (MoveForm ox oy dx dy) -> do let move = Move (1 + (Prelude.length $ _history $ cd)) (ox,oy) (dx,dy) 
                                                                                     case makeMove move cd of 
                                                                                          Valid cd' -> do runDB $ do update aiGameId [AiGameHistory =. (historyToText $ (_history cd'))]
                                                                                                                     update aiGameId [AiGameGameStatus =. (_status cd')]
                                                                                                          redirect (AiGameR aiGameId)
                                                                                          Invalid r -> do setMessage $ toHtml ("Move invalid: " Prelude.++ show r)
                                                                                                          redirect (AiGameR aiGameId)
                                            FormFailure f -> do setMessage $ toHtml ("Failure " Prelude.++ show f)
                                                                redirect (AiGameR aiGameId)
                                            FormMissing -> do setMessage $ toHtml ("Form Missing" :: Text)
                                                              redirect (AiGameR aiGameId)
                            False -> do setMessage $ toHtml ("Not authorized to make this move" :: Text)
                                        redirect (AiGameR aiGameId)
