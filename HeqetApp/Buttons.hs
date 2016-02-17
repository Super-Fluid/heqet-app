module HeqetApp.Buttons where

import HeqetApp.Types

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Events
import Data.IORef
import Control.Lens hiding ((#),set)

{-
A "simple" button acts on the selected music
without even seeing the other music. 
-}
simpleButton :: String -> (Music -> Music) -> IORef AppState -> UI Element
simpleButton desc f state = do
    b <- UI.button # set text desc
    on click b $ const $ do
        old <- liftIO $ readIORef state
        liftIO $ writeIORef state (old & _2 %~ f) -- TODO: only selected
    return b

{-
A "total" button acts on all the music.
For example, changing the selection. 
-}
totalButton :: String -> (AppState -> AppState) -> IORef AppState -> UI Element
totalButton desc f state = do
    b <- UI.button # set text desc
    on click b $ const $ do
        old <- liftIO $ readIORef state
        liftIO $ writeIORef state $ f old
    return b

{-
A "UI" button can look at settings and other UI stuff.
Well, you could use liftIO to do anything, but please don't.
-}
uiSimpleButton :: String -> (Music -> UI Music) -> IORef AppState -> UI Element
uiSimpleButton desc f state = do
    b <- UI.button # set text desc
    on click b $ const $ do
        (i,old) <- liftIO $ readIORef state
        new <- f old -- TODO: only selected
        liftIO $ writeIORef state (i,new)
    return b

uiTotalButton :: String -> (AppState -> UI AppState) -> IORef AppState -> UI Element
uiTotalButton desc f state = do
    b <- UI.button # set text desc
    on click b $ const $ do
        old <- liftIO $ readIORef state
        new <- f old
        liftIO $ writeIORef state new
    return b

{-
An "IO" button (always total) can do is the 
same type as a UI button, but is allowed 
to do IO.
-}
ioButton :: String -> (AppState -> UI AppState) -> IORef AppState -> UI Element
ioButton desc f state = do
    b <- UI.button # set text desc
    on click b $ const $ do
        old <- liftIO $ readIORef state
        new <- f old
        liftIO $ writeIORef state new
    return b

makeDefaultPanel :: (IORef AppState)  -> String -> [(IORef AppState -> UI Element)] -> UI Element
makeDefaultPanel state s items = do
    p <- UI.div #. "panel"
    heading <- UI.div # set text s #. "panel-heading"
    contents <- UI.div #+ map ($ state) items
    return p #+ [return heading, return contents #. "panel-contents"]
    return p

filterRow :: String -> (Comparison -> String -> AppState -> AppState) -> IORef AppState -> UI Element
filterRow desc f state = do
    compareToInput <- UI.input
    
    let makeButton comp tcomp = do
        b <- UI.button # set text tcomp
        on click b $ const $ do
            old <- liftIO $ readIORef state
            compareToValue <- compareToInput # get value
            let new = f comp compareToValue old
            liftIO $ writeIORef state new
        return b
    
    ble <- makeButton LE "<"
    bleq <- makeButton LEQ "<="
    bq <- makeButton Equal "=="
    bgeq <- makeButton GEQ ">="
    bge <- makeButton GE ">"
    
    row 
        [ string desc
        , return ble, return bleq, return bq, return bgeq, return bge
        , return compareToInput
        ]

addRemoveRow :: String -> (AddRemove -> Music -> Music) -> IORef AppState -> UI Element
addRemoveRow obj f state = row
    [ simpleButton ("add "++obj) (f Add) state
    , simpleButton ("remove "++obj) (f Remove) state
    ]
