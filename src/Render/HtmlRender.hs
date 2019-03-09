{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}

module Render.HtmlRender (renderChessContainer, moveForm, MoveForm(MoveForm)) where


import Import
import Logic.ChessData
import Data.Array
import Data.Text as DT
import Control.Lens


renderChessContainer :: ChessData -> Widget
renderChessContainer cd = do toWidget [whamlet| <div .container id="chesscontainer">
                                                   ^{renderBoard $ _board cd}
                                                   ^{renderOffPieces $ _offPieces cd}|]

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

renderSquare :: (Int, Int) -> Square -> Widget
renderSquare (x, y) e = case e of 
                            None -> renderSquareHelper [shamlet||]
                            (Ent col pc) -> renderSquareHelper $ do toWidget [hamlet|<img src=@{iconAccessor col pc} id="icon">|]
                                                                    toWidget [lucius| #icon {
                                                                                        width: 100%;
                                                                                        height: 100%;} |]
 where  renderSquareHelper :: Widget -> Widget
        renderSquareHelper inner = do idtag <- newIdent
                                      let funstring = "visclick(" Prelude.++ show x Prelude.++ "," Prelude.++ show y Prelude.++ ")"
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
renderBoard arr = do toWidget [whamlet| <div id="chessboard">
                                            <table id="chesstable">
                                                $forall y <- ys
                                                    <tr>
                                                        $forall x <- xs
                                                            <td> ^{renderSquare (x, y) (arr ! (x, y))}                
                                |]
                    where xs = [1..8]
                          ys = [8, 7..1]

renderOffPieces :: [(Piece, Color)] -> Widget
renderOffPieces opcs = do toWidget [whamlet| <div id="offpieces">
                                                 <table>
                                                    <tr> 
                                                        $forall (wp, wc) <- whites
                                                            <td>
                                                                <div id="piececontainer">
                                                                    <img src=@{iconAccessor wc wp} id="icon" style="margin: auto">
                                                    <tr>
                                                        $forall (bp, bc) <- blacks
                                                            <td>
                                                                <div id="piececontainer">
                                                                    <img src=@{iconAccessor bc bp} id="icon" style="margin: auto">     |] 
                          toWidget [lucius| #piececontainer {
                                                 width: 100px;
                                                 height: 100px;
                                                 min-height: 100px;
                                            }
                                   |] -- @TODO dont hardcode the pixel size
                         where whites = (Prelude.filter (\(_, c) -> (c == White)) opcs)
                               blacks = (Prelude.filter (\(_, c) -> (c == Black)) opcs)