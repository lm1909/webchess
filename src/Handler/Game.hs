{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Game where

import           Control.Applicative
import Import
import Logic.ChessData
import Data.Array
import Logic.ChessDBConnector
import Logic.ChessLegal
import Data.Text as DT
import Text.Julius (RawJS (..))
import Logic.Chess
import Control.Lens
import Render.HtmlRender
import Logic.Ai


gameToChessData :: Game -> ChessData
gameToChessData game = gameFromMoves (textToHistory $ gameHistory game)

getGameR :: GameId -> Handler Html
getGameR gameId = do game <- runDB $ get404 gameId
                     (id, user) <- requireAuthPair
                     ((res, movewidget), enctype) <- runFormGet moveForm
                     let cd = gameToChessData game
                     (player, opponent) <- runDB $ do player <- get404 (gamePlayer game) -- @TODO is a 404 really optimal here?
                                                      opponent <- get404 (gameOpponent game)
                                                      return (player, opponent)
                     defaultLayout $ do setTitle "Game"
                                        addScriptRemote "http://code.jquery.com/jquery-latest.js" -- this is necessary for the live update view js
                                        $(widgetFile "game")


postGameR :: GameId -> Handler Html
postGameR gameId = do ((result, widget), enctype) <- runFormPostNoToken moveForm -- @TODO enable cross site request forgery protection
                      game <- runDB $ get404 gameId
                      (id, user) <- requireAuthPair
                      (player, opponent) <- runDB $ do player <- get404 (gamePlayer game) -- @TODO is a 404 really optimal here?
                                                       opponent <- get404 (gameOpponent game)
                                                       return (player, opponent)
                      let cd = gameToChessData game
                      case (((id == gamePlayer game) && (cd^.playerOnTurn == White)) || ((id == gameOpponent game) && (cd^.playerOnTurn == Black))) of
                        True -> case result of
                                        FormSuccess (MoveForm ox oy dx dy) -> do let move = Move (1 + (Prelude.length $ _history $ cd)) (ox,oy) (dx,dy) 
                                                                                 case makeMove move cd of 
                                                                                    Valid cd' -> do runDB $ do update gameId [GameHistory =. (historyToText $ (_history cd'))]
                                                                                                               update gameId [GameGameStatus =. (_status cd')]
                                                                                                    redirect (GameR gameId)
                                                                                    Invalid r -> do setMessage $ toHtml ("Move invalid: " Prelude.++ show r)
                                                                                                    redirect (GameR gameId)
                                        FormFailure f -> do setMessage $ toHtml ("Failure " Prelude.++ show f)
                                                            redirect (GameR gameId)
                                        FormMissing -> do setMessage $ toHtml ("Form Missing" :: Text)
                                                          redirect (GameR gameId)
                        False -> do setMessage $ toHtml ("Not authorized to make this move" :: Text)
                                    redirect (GameR gameId)

