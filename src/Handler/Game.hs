{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Game where

import           Control.Applicative
import Import
import Logic.ChessData
import Data.Array

data MoveForm = MoveForm {ox :: Int, oy :: Int, dx :: Int, dy :: Int}

moveForm :: Html -> MForm Handler (FormResult MoveForm, Widget)
moveForm extra = do
    (oxRes, oxView) <- mreq intField "oxView" Nothing
    (oyRes, oyView) <- mreq intField "oyView" Nothing
    (dxRes, dxView) <- mreq intField "dxView" Nothing
    (dyRes, dyView) <- mreq intField "dyView" Nothing
    let moveRes = MoveForm <$> oxRes <*> oyRes <*> dxRes <*> dyRes
    let widget = do toWidget [whamlet| #{extra}
                                       <div .container>
                                           <div .row>
                                             <div .col-md-1>
                                                 <b>Move
                                             <div .col .justify-content-end #coords>
                                                 <div .justify-content-end> From (^{fvInput oxView}, ^{fvInput oyView}) to (^{fvInput dxView}, ^{fvInput dyView})
                                           <button .button class="btn btn-primary btn-lg btn-block" id="movebutton">Move
                             |]
                    toWidget [lucius| #movebutton {
                                        margin: 10px 0px 10px 0px;              
                                      }
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

renderSquare :: (Int, Int) -> Square -> Widget
renderSquare (x, y) e = case e of 
                            None -> renderSquareHelper [shamlet||]
                            (Ent col pc) -> renderSquareHelper $ do toWidget [hamlet|<img src=@{iconAccessor col pc} id="icon">|]
                                                                    toWidget [lucius| #icon {
                                                                                        width: 100%;
                                                                                        height: 100%;} |]
 where  renderSquareHelper :: Widget -> Widget
        renderSquareHelper inner = do idtag <- newIdent
                                      toWidget [whamlet| <div .border id=#{idtag}> ^{inner}|]
                                      toWidget [lucius| ##{idtag} {
                                                           width: 100px;
                                                           height: 100px;
                                                           min-height: 60px;
                                                           background-color: #{color}; 
                                          }|] -- @TODO make the size of the board adaptable (eg dont hardcode 80px)
                                       where color = if (((x+y) `mod` 2) == 1) then ("#f8f9fa" :: String) else ("#6c757d" :: String)
                                        -- colors taken from bootstrap standard palette

iconAccessor :: Color -> Piece -> Route App
iconAccessor Black King = StaticR svg_blackking_svg
iconAccessor Black Queen = StaticR svg_blackqueen_svg
iconAccessor Black Pawn = StaticR svg_blackpawn_svg
iconAccessor Black Bishop = StaticR svg_blackbishop_svg
iconAccessor Black Rook = StaticR svg_blackrook_svg
iconAccessor Black Knight = StaticR svg_blackknight_svg
iconAccessor White King = StaticR svg_whiteking_svg
iconAccessor White Queen = StaticR svg_whitequeen_svg
iconAccessor White Pawn = StaticR svg_whitepawn_svg
iconAccessor White Bishop = StaticR svg_whitebishop_svg
iconAccessor White Rook = StaticR svg_whiterook_svg
iconAccessor White Knight = StaticR svg_whiteknight_svg

renderBoard :: Board -> Widget
renderBoard arr = do toWidget [whamlet| <table id="chesstable">
                                            $forall y <- ys
                                                <tr>
                                                    $forall x <- xs
                                                        <td> ^{renderSquare (x, y) (arr ! (x, y))}                
                                |]
                    where xs = [1..8]
                          ys = [1..8]











