{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Profile where

import           Import

data AccountData = AccountData {nick :: Text} deriving Show

accountForm :: Maybe Text -> Form AccountData
accountForm maybename = renderBootstrap $ AccountData
                <$> areq textField "Nickname: " maybename

getProfileR :: Handler Html
getProfileR = do
    (authid, user) <- requireAuthPair
    person <- runDB $ get404 authid
    (widget, enctype) <- generateFormPost (accountForm (Just $ userNick person))
    defaultLayout $ do
        setTitle "User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do ((result, widget), enctype) <- runFormPost (accountForm Nothing)

                  (authid, user) <- requireAuthPair
                  (person) <- runDB $ get authid

                  case result of
                    FormSuccess account -> do runDB $ update authid [UserNick =. (nick account)] -- @TODO need to check here that username is unique
                                              setMessage $ toHtml ("Updated User nickname" :: Text)
                                              defaultLayout $ $(widgetFile "profile")
                    _ -> defaultLayout $ $(widgetFile "profile")
