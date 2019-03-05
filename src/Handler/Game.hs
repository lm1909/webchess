{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Game where

import           Control.Applicative
import Import

data Move = Move {ox :: Int, oy :: Int, dx :: Int, dy :: Int}

moveForm :: Html -> MForm Handler (FormResult Move, Widget)
moveForm extra = do
    (oxRes, oxView) <- mreq intField "oxView" Nothing
    (oyRes, oyView) <- mreq intField "oyView" Nothing
    (dxRes, dxView) <- mreq intField "dxView" Nothing
    (dyRes, dyView) <- mreq intField "dyView" Nothing
    let moveRes = Move <$> oxRes <*> oyRes <*> dxRes <*> dyRes
    let widget = do [whamlet| #{extra}
                              <p><b>Move</b> From (^{fvInput oxView}, ^{fvInput oyView}) to (^{fvInput dxView}, ^{fvInput dyView})
                              <button .button class="btn btn-primary btn-lg btn-block">Move
                    |]
    return (moveRes, widget)

getGameR :: GameId -> Handler Html
getGameR gameId = do game <- runDB $ get404 gameId
                     ((res, movewidget), enctype) <- runFormGet moveForm
                     defaultLayout $ do setTitle "Game"
                                        $(widgetFile "game")


postGameR :: GameId -> Handler Html
postGameR gameId = do ((result, widget), enctype) <- runFormPostNoToken moveForm -- @TODO enable cross site request forgery protection
                      case result of
                        FormSuccess move -> do setMessage $ toHtml ("Move sent" :: Text)
                                               redirect (GameR gameId) -- @TODO update game state
                        FormFailure f -> do setMessage $ toHtml ("Failure " Prelude.++ show f)
                                            redirect (GameR gameId)
                        FormMissing -> do setMessage $ toHtml ("Form Missing" :: Text)
                                          redirect (GameR gameId)
