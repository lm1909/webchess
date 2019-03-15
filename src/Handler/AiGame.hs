{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoImplicitPrelude     #-}

{-|
Module      : Handler.AiGame

The AiGame module contains the Handler for getting an AI game and describes the high level 'protocol' of an AI game
-}

module Handler.AiGame where

import           Control.Concurrent
import           Control.Lens
import           Import

import           Logic.Ai
import           Logic.Chess
import           Logic.ChessData
import           Logic.ChessDBConnector
import           Logic.ChessLegal
import           Logic.ChessOutput
import           Render.HtmlRender


aiGameToChessData :: AiGame -> ChessData
aiGameToChessData aigame = gameFromMoves (textToHistory $ aiGameHistory aigame)

getAiGameR :: AiGameId -> Handler Html
getAiGameR aiGameId = do aigame <- runDB $ get404 aiGameId
                         mmsg <- getMessage
                         (authid, _) <- requireAuthPair
                         ((res, movewidget), enctype) <- runFormGet moveForm
                         let cd = aiGameToChessData aigame
                         player <- runDB $ get404 (aiGamePlayer aigame) -- @TODO is a 404 really optimal here?
                         let moveauthorized = authid == aiGamePlayer aigame
                         defaultLayout $ do setTitle "AI Match"
                                            $(widgetFile "aigame")

postAiGameR :: AiGameId -> Handler Html
postAiGameR aiGameId = do ((result, widget), enctype) <- runFormPostNoToken moveForm -- @TODO enable cross site request forgery protection
                          mmsg <- getMessage
                          aigame <- runDB $ get404 aiGameId
                          (authid, _) <- requireAuthPair
                          let cd = aiGameToChessData aigame
                          case (((authid == aiGamePlayer aigame) && (cd^.playerOnTurn == White))) of
                            True -> case result of
                                            FormSuccess (MoveForm ox oy dx dy) -> do let move = Move (1 + (length $ cd^.history)) (ox,oy) (dx,dy)
                                                                                     case makeMove move cd of
                                                                                          Valid cd' -> do runDB $ do update aiGameId [AiGameHistory =. (historyToText $ (cd'^.history))]
                                                                                                                     update aiGameId [AiGameGameStatus =. (cd'^.status)]
                                                                                                                     update aiGameId [AiGameThinking =. True]
                                                                                                          runInnerHandler <- handlerToIO
                                                                                                          _ <- liftIO $ forkIO $ runInnerHandler $ do
                                                                                                              let cd'' = setMove (bestMove (aiGameDiff aigame) cd') cd' -- AI calculates & does its move
                                                                                                              runDB $ do update aiGameId [AiGameGameStatus =. (cd''^.status)]
                                                                                                                         update aiGameId [AiGameHistory =. (historyToText $ (cd''^.history))]
                                                                                                                         update aiGameId [AiGameThinking =. False]
                                                                                                          redirect (AiGameR aiGameId)
                                                                                          Invalid r -> do setMessage $ toHtml ("Move invalid: " ++ display r)
                                                                                                          redirect (AiGameR aiGameId)
                                            FormFailure f -> do setMessage $ toHtml ("Failure " ++ show f)
                                                                redirect (AiGameR aiGameId)
                                            FormMissing -> do setMessage $ toHtml ("Form Missing" :: Text)
                                                              redirect (AiGameR aiGameId)
                            False -> do setMessage $ toHtml ("Not authorized to make this move" :: Text)
                                        redirect (AiGameR aiGameId)

-- | Widget that displays the spinner / thinking status of the AI
aiWaitSpinner :: Widget
aiWaitSpinner = do toWidget [whamlet|
                                <div .row #waitbox>
                                    <div .col-lg-2 .spinner>
                                    <div .col-lg-8 #aithinkingtext>
                                        <h3> AI is thinking..
                            |]
                   toWidget [cassius| #waitbox
                                        margin: auto;
                                        padding: 10px;
                                        display: inline-flex;
                            |]
                   toWidget [cassius| #aithinkingtext
                                        align-text: right;
                            |]
                   addScriptRemote "http://code.jquery.com/jquery-latest.js" -- this is necessary for the live update view js
                   $(widgetFile "autoload-aistatus")
                   toWidget [lucius| .spinner {
                                           /* Spinner size and color */
                                           width: 5rem;
                                           height: 5rem;
                                           border-top-color: #6c757d;
                                           border-left-color: #6c757d;

                                           /* Additional spinner styles */
                                           animation: spinner 1700ms linear infinite;
                                           border-bottom-color: transparent;
                                           border-right-color: transparent;
                                           border-style: solid;
                                           border-width: 2px;
                                           border-radius: 50%;
                                           box-sizing: border-box;
                                           display: inline-block;
                                           vertical-align: middle;
                                     }

                                     /* Animation styles */
                                     @keyframes spinner {
                                       0% { transform: rotate(0deg); }
                                       100% { transform: rotate(360deg); }
                                     }
                            |]
