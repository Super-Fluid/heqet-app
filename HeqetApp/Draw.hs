module HeqetApp.Draw where

{-
Functions to draw a Heqet list of symbols to
an html5 canvas using the Threepenny-GUI interface.

-}

import HeqetApp.Types

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes

draw :: UI Element -> [HeadingSymbol] -> [Symbol] -> ViewState -> UI ()
draw canvas hsyms syms view = return ()

