{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Profile where

import Import

data AccountData = AccountData {nick :: Text} deriving Show

accountForm :: Maybe Text -> Form AccountData
accountForm maybename = renderBootstrap $ AccountData
                <$> areq textField "Nickname: " maybename

getProfileR :: Handler Html
getProfileR = do
    (id, user) <- requireAuthPair
    person <- runDB $ get404 id
    (widget, enctype) <- generateFormPost (accountForm (userNick person))
    defaultLayout $ do
        setTitle "User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do ((result, widget), enctype) <- runFormPost (accountForm Nothing)

                  (id, user) <- requireAuthPair
                  (person) <- runDB $ get id

                  case result of
                    FormSuccess account -> do runDB $ update id [UserNick =. (Just $ nick account)]
                                              setMessage $ toHtml ("Updated User nickname" :: Text)
                                              defaultLayout $ $(widgetFile "profile")
                    _ -> defaultLayout $ $(widgetFile "profile")
