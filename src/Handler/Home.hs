{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Handler.Home

Generic handler to serve homepage
-}
module Handler.Home where

import           Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do setTitle "webchess - Home"
                              $(widgetFile "homepage")
