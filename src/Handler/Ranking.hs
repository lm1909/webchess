{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Handler.Ranking

Handler to produce highscore list of all webchess users
-}

module Handler.Ranking where

import           Import

getRankingR :: Handler Html
getRankingR = do besthundredplayers <- runDB $ selectList [UserElo >. 1000] [Desc UserElo]
                 defaultLayout $ do setTitle "Ranking"
                                    $(widgetFile "ranking")
-- getRankingR = defaultLayout $ do setTitle "Ranking"
--                                  $(widgetFile "homepage")

showPlayer :: Entity User -> String
showPlayer (Entity key player) = (show $ userNick player) ++ " with an elo of " ++ (show $ userElo player)

keyIdPlayer :: Entity User -> Key User
keyIdPlayer (Entity key _) = key
