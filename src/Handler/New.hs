{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.New where

import Import

data GameMeta = GameMeta {opponent :: Int} deriving Show

gameForm :: Form GameMeta
gameForm = renderBootstrap $ GameMeta 
                <$> areq intField "Opponent UserId" Nothing

getNewR :: Handler Html
getNewR = do (widget, enctype) <- generateFormPost gameForm
             defaultLayout $ do setTitle "New Game"
                                $(widgetFile "new")

postNewR :: Handler Html
postNewR = do ((result, widget), enctype) <- runFormPost gameForm
              case result of
                FormSuccess game -> redirect (HomeR) -- @TODO real game id
                _ -> defaultLayout $ $(widgetFile "new")

