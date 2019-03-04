{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Game where

import Import

getGameR :: GameId -> Handler Html
getGameR gameId = do game <- runDB $ get404 gameId
                     defaultLayout $ do setTitle "Game"
                                        $(widgetFile "game")

postGameR :: GameId -> Handler Html
postGameR gameId = error "Not yet implemented: postGameR"
