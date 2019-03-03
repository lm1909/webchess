{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.User where

import Import

getUserR :: UserId -> Handler Html
getUserR personIdent = do person <- runDB $ get404 personIdent
                          defaultLayout $ do setTitle "User"
                                             $(widgetFile "user")
