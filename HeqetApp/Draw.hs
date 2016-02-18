module HeqetApp.Draw 
    ( draw
    ) where

{-
Functions to draw a Heqet list of symbols to
an html5 canvas using the Threepenny-GUI interface.

-}

import HeqetApp.Types

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Canvas
import Control.Lens hiding ((#),set')

draw :: [HeadingSymbol] -> [Symbol] -> ViewState -> Canvas -> UI ()
draw hsyms syms viewstate canvas = do
    canvas # dot (300,300) (viewstate^.staffSize)
    return ()

dot :: (PX,PX) -> PX -> Canvas -> UI ()
dot point scale canvas = do
    canvas # beginPath
    canvas # arc point 2 0 7
    canvas # closePath
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # fill
    return ()