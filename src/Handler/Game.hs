{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Game where

import           Logic.Ai
import           Logic.Chess
import           Logic.ChessData
import           Logic.ChessDBConnector
import           Logic.ChessLegal
import           Logic.Elo
import           Render.HtmlRender

import           Control.Lens
import           Import


gameToChessData :: Game -> ChessData
gameToChessData game = gameFromMoves (textToHistory $ gameHistory game)

getGameR :: GameId -> Handler Html
getGameR gameId = do game <- runDB $ get404 gameId
                     (authid, user) <- requireAuthPair
                     ((res, movewidget), enctype) <- runFormGet moveForm
                     let cd = gameToChessData game
                     (player, opponent) <- runDB $ do player <- get404 (gamePlayer game) -- @TODO is a 404 really optimal here?
                                                      opponent <- get404 (gameOpponent game)
                                                      return (player, opponent)
                     let moveauthorized = (authid == gamePlayer game) || (authid == gameOpponent game)
                     defaultLayout $ do setTitle "Game"
                                        $(widgetFile "game")


postGameR :: GameId -> Handler Html
postGameR gameId = do ((result, widget), enctype) <- runFormPostNoToken moveForm -- @TODO enable cross site request forgery protection
                      game <- runDB $ get404 gameId
                      (authid, _) <- requireAuthPair
                      (player, opponent) <- runDB $ do player <- get404 (gamePlayer game) -- @TODO is a 404 really optimal here?
                                                       opponent <- get404 (gameOpponent game)
                                                       return (player, opponent)
                      let cd = gameToChessData game
                      case (((authid == gamePlayer game) && (cd^.playerOnTurn == White)) || ((authid == gameOpponent game) && (cd^.playerOnTurn == Black))) of
                        True -> case result of
                                        FormSuccess (MoveForm ox oy dx dy) -> do let move = Move (1 + (Prelude.length $ _history $ cd)) (ox,oy) (dx,dy)
                                                                                 case makeMove move cd of
                                                                                    Valid cd' -> do runDB $ do update gameId [GameHistory =. (historyToText $ (_history cd'))]
                                                                                                               update gameId [GameGameStatus =. (_status cd')]
                                                                                                    case (cd'^.status) of 
                                                                                                        Running -> redirect (GameR gameId)
                                                                                                        (Finished result) | (gameElocalcoutstanding game) -> do let (elowhite, eloblack) = eloUpdate result (userElo player) (userElo opponent) 
                                                                                                                                                                runDB $ do update (gamePlayer game) [UserElo =. elowhite]
                                                                                                                                                                           update (gameOpponent game) [UserElo =. eloblack]
                                                                                                                                                                redirect (GameR gameId)
                                                                                                                          | otherwise -> redirect (GameR gameId)
                                                                                    Invalid r -> do setMessage $ toHtml ("Move invalid: " Prelude.++ show r)
                                                                                                    redirect (GameR gameId)
                                        FormFailure f -> do setMessage $ toHtml ("Failure " Prelude.++ show f)
                                                            redirect (GameR gameId)
                                        FormMissing -> do setMessage $ toHtml ("Form Missing" :: Text)
                                                          redirect (GameR gameId)
                        False -> do setMessage $ toHtml ("Not authorized to make this move" :: Text)
                                    redirect (GameR gameId)


