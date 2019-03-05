{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.New where

import Import

data GameMeta = GameMeta {opponent :: Text} deriving Show

gameForm :: Form GameMeta
gameForm = renderBootstrap $ GameMeta 
                <$> areq textField "Opponent Nickname" Nothing

getNewR :: Handler Html
getNewR = do (widget, enctype) <- generateFormPost gameForm
             defaultLayout $ do setTitle "New Game"
                                $(widgetFile "new")

postNewR :: Handler Html
postNewR = do ((result, widget), enctype) <- runFormPost gameForm
              case result of
                FormSuccess game -> do (id, user) <- requireAuthPair
                                       opponent <- runDB $ getBy $ UniqueNick (opponent game)
                                       case opponent of 
                                            Nothing -> do setMessage $ toHtml ("Game creation failed: No such user exists" :: Text)
                                                          defaultLayout $ $(widgetFile "new")
                                            (Just (Entity key val)) -> do gameid <- runDB $ insert Game {gamePlayer = id, gameOpponent = key}
                                                                          redirect (GameR gameid) 
                _ -> defaultLayout $ $(widgetFile "new")

