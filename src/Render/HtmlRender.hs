{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Render.HtmlRender

HtmlRender contains all code related to rendering the chess game stage in html
-}
module Render.HtmlRender (renderGameStage, moveForm, MoveForm(MoveForm)) where

import           Data.Array
import           Import

import           Control.Lens
import           Logic.ChessData
import           Logic.ChessOutput

data MoveForm = MoveForm {ox :: Int, oy :: Int, dx :: Int, dy :: Int}

-- | the monadic form used to enter move information
moveForm :: Html -> MForm Handler (FormResult MoveForm, Widget)
moveForm extra = do
    (oxRes, oxView) <- mreq intField "oxView" Nothing
    (oyRes, oyView) <- mreq intField "oyView" Nothing
    (dxRes, dxView) <- mreq intField "dxView" Nothing
    (dyRes, dyView) <- mreq intField "dyView" Nothing
    let moveRes = MoveForm <$> oxRes <*> oyRes <*> dxRes <*> dyRes
    let widget = do toWidget [whamlet| #{extra}
                                       <div .container #controlcontainer>
                                           <div .row>
                                             <div .col-md-1>
                                                 <b>Move
                                             <div .col .align-self-end #coords>From (^{fvInput oxView}, ^{fvInput oyView}) to (^{fvInput dxView}, ^{fvInput dyView})
                                           <button .button .btn .btn-primary .btn-lg .btn-block #movebutton>Move
                             |]
                    toWidget [cassius| #movebutton
                                            margin: 10px 0px 10px 0px;
                             |]
                    toWidget [cassius| #controlcontainer
                                            padding: 10px;
                             |]
                    toWidget [cassius| #coords
                                            text-align: right;
                                            padding-right: 25px;
                             |]
    return (moveRes, widget)

-- | renders a single square of a chessboard in html
renderSquare :: (Int, Int) -> Square -> Widget
renderSquare (x, y) e = case e of
                            None -> renderSquareHelper [shamlet||]
                            (Ent col pc) -> renderSquareHelper $ do toWidget [hamlet|<img src=@{iconAccessor col pc} #icon>|]
                                                                    toWidget [cassius| #icon
                                                                                           width: 100%;
                                                                                           height: 100%;
                                                                             |]
 where  renderSquareHelper :: Widget -> Widget
        renderSquareHelper inner = do idtag <- newIdent
                                      let funstring = "visclick(" ++ show x ++ "," ++ show y ++ ")"
                                      toWidget [whamlet| <div .border id=#{idtag} onclick="#{funstring}"> ^{inner}|]
                                      toWidget [lucius| ##{idtag} {
                                                           width: 100px;
                                                           height: 100px;
                                                           min-height: 60px;
                                                           background-color: #{color};
                                          }|] -- @TODO make the size of the board adaptable (eg dont hardcode 100px)
                                      toWidget [julius|
                                        var fst = true;
                                        function visclick(x, y){
                                            if (fst){
                                                document.getElementById('hident2').value = x;
                                                document.getElementById('hident3').value = y;
                                                document.getElementById('hident4').value = '';
                                                document.getElementById('hident5').value = '';
                                                fst = false;
                                            } else {
                                                document.getElementById('hident4').value = x;
                                                document.getElementById('hident5').value = y;
                                                fst = true;
                                            }
                                        }
                                      |] -- @TODO hident is not stable, this is very ugly
                                       where color = if (((x+y) `mod` 2) == 1) then ("#f8f9fa" :: String) else ("#6c757d" :: String)
                                        -- colors taken from bootstrap standard palette

-- | convenience staticR icon accessor function
iconAccessor :: Color -> Piece -> Route App
iconAccessor Black King   = StaticR svg_blackking_svg
iconAccessor Black Queen  = StaticR svg_blackqueen_svg
iconAccessor Black Pawn   = StaticR svg_blackpawn_svg
iconAccessor Black Bishop = StaticR svg_blackbishop_svg
iconAccessor Black Rook   = StaticR svg_blackrook_svg
iconAccessor Black Knight = StaticR svg_blackknight_svg
iconAccessor White King   = StaticR svg_whiteking_svg
iconAccessor White Queen  = StaticR svg_whitequeen_svg
iconAccessor White Pawn   = StaticR svg_whitepawn_svg
iconAccessor White Bishop = StaticR svg_whitebishop_svg
iconAccessor White Rook   = StaticR svg_whiterook_svg
iconAccessor White Knight = StaticR svg_whiteknight_svg

-- | render entire chess board in html
renderBoard :: Board -> Widget
renderBoard arr = do toWidget [whamlet| <div #chessboard>
                                            <table #chesstable>
                                                $forall y <- ys
                                                    <tr>
                                                        $forall x <- xs
                                                            <td> ^{renderSquare (x, y) (arr ! (x, y))}
                                |]
                     toWidget [cassius| #chesstable
                                            margin: auto;
                              |]
                    where xs = [1..8]
                          ys = [8, 7..1] -- NOTE: watch orientation of chessboard

-- | renders all off pieces (e.g. pieces that already were taken)
renderOffPieces :: [(Piece, Color)] -> Widget
renderOffPieces opcs = do toWidget [whamlet| <div #offpieces>
                                                 <table>
                                                    <tr>
                                                        $forall (wp, wc) <- whites
                                                            <td>
                                                                <div #piececontainer>
                                                                    <img src=@{iconAccessor wc wp} #icon style="margin: auto">
                                                    <tr>
                                                        $forall (bp, bc) <- blacks
                                                            <td>
                                                                <div #piececontainer>
                                                                    <img src=@{iconAccessor bc bp} #icon style="margin: auto">
                                   |]
                          toWidget [cassius| #piececontainer
                                                width: 100px;
                                                height: 100px;
                                                min-height: 100px;|]
                          toWidget [cassius| #offpieces
                                                margin: auto;
                                                padding: 10px;
                                   |] -- @TODO dont hardcode the pixel size
                         where whites = (filter (\(_, c) -> (c == White)) opcs)
                               blacks = (filter (\(_, c) -> (c == Black)) opcs)

-- | renders the entire chess container in html
renderChessContainer :: ChessData -> Widget
renderChessContainer cd = do toWidget [whamlet| <div .container #chesscontainer>
                                                    <div #onturn>
                                                        <h2 .centered #onturn> Player on turn is <b> #{show $ _playerOnTurn cd} </b>
                                                    ^{renderBoard $ _board cd}
                                                    ^{renderOffPieces $ _offPieces cd}|]
                             toWidget [cassius| #onturn
                                                    text-align: center;
                                      |]
                             toWidget [cassius| #chesscontainer
                                                    margin-left: auto;
                                                    margin-right: auto;
                                      |]

renderGameStage :: Bool -> ChessData -> Widget -> Enctype -> Widget
renderGameStage moveauth cd moveformwidget enctype = do toWidget [whamlet| <div .container>
                                                                              <div .container #exTab>
                                                                                  <ul .nav .nav-tabs>
                                                                                      <li .active>
                                                                                          <a href="#game" data-toggle="tab"> Game
                                                                                      <li>
                                                                                          <a href="#history" data-toggle="tab"> History

                                                                                  <div .tab-content>
                                                                                      <div .tab-pane .active #game>
                                                                                          <div .row>
                                                                                              ^{renderChessContainer cd}

                                                                                          <div .row #winnotice>
                                                                                            $case (_status cd)
                                                                                                $of (Finished (Winner col))
                                                                                                    <h1 .text-center> Game finished! Winner: #{show col}
                                                                                                $of _

                                                                                          <div #interactionbox .row>
                                                                                              $case (_status cd)
                                                                                                  $of (Running)
                                                                                                      $if (moveauth)
                                                                                                          <form method=post action="#" enctype=#{enctype}>
                                                                                                              ^{moveformwidget}
                                                                                                  $of _

                                                                                      <div .tab-pane #history>
                                                                                          <h2> All moves in the game:
                                                                                          <ul .list-group>
                                                                                              $forall m <- _history cd
                                                                                                  <li .list-group-item>#{display m}
                                                                 |]
                                                        toWidget [cassius| #winnotice
                                                                                margin: auto;
                                                                 |]
                                                        toWidget [cassius| #exTab
                                                                                color: whiteM
                                                                                padding: 2% 0px 2% 0px;
                                                                 |]
                                                        addScriptRemote "http://code.jquery.com/jquery-latest.js" -- this is necessary for the live update view js
                                                        $(widgetFile "autoload")
