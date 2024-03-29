{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Handler.Profile

Handler to show profile of user and change information
-}
module Handler.Profile where

import           Import

data AccountData = AccountData {nick :: Text} deriving Show

accountForm :: Maybe Text -> Form AccountData
accountForm maybename = renderBootstrap $ AccountData
                <$> areq textField "Nickname: " maybename

getProfileR :: Handler Html
getProfileR = do
    (authid, user) <- requireAuthPair
    mmsg <- getMessage
    person <- runDB $ get404 authid
    (widget, enctype) <- generateFormPost (accountForm (Just $ userNick person))
    defaultLayout $ do
        setTitle "User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do ((result, widget), enctype) <- runFormPost (accountForm Nothing)
                  mmsg <- getMessage

                  (authid, user) <- requireAuthPair
                  (person) <- runDB $ get authid

                  case result of
                    FormSuccess account -> do already <- runDB $ getBy $ (UniqueNick (nick account))
                                              case already of 
                                                Nothing -> do runDB $ update authid [UserNick =. (nick account)]
                                                              setMessage $ toHtml ("Updated User nickname" :: Text)
                                                              redirect ProfileR
                                                _ -> do setMessage $ toHtml ("Username already taken" :: Text)
                                                        redirect ProfileR
                    _ -> defaultLayout $ do setMessage $ toHtml ("Form input invalid" :: Text)
                                            redirect ProfileR
