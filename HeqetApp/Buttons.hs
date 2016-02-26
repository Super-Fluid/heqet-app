module HeqetApp.Buttons where

import HeqetApp.Types

import Heqet.Types

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Events
import Data.IORef
import Control.Lens hiding ((#),set)
import Control.Monad

{-
A "simple" button acts on the selected music
without even seeing the other music. 
-}
simpleButton :: String -> (Music -> Music) -> UI (Event [Mutator],UI Element)
simpleButton desc f = do
    b <- UI.button # set text desc
    let eMutators = (const $ PureMutator $ makeAction f) <$> click b 
    return (fmap return eMutators,return b)

{-
A "total" button acts on all the music.
For example, changing the selection. 
-}
totalButton :: String -> (AppState -> AppState) -> UI (Event [Mutator],UI Element)
totalButton desc f = do
    b <- UI.button # set text desc
    let eMutators = (const $ PureMutator f) <$> click b 
    return (fmap return eMutators,return b)

colorButton :: String -> String -> (AppState -> AppState) -> UI (Event [Mutator],UI Element)
colorButton desc bid f = do
    b <- UI.button # set text desc #. ("color-"++bid)
    let eMutators = (const $ PureMutator f) <$> click b 
    return (fmap return eMutators,return b)

viewButton :: String -> (ViewState -> ViewState) -> UI (Event (ViewState -> ViewState),UI Element)
viewButton desc f = do
    b <- UI.button # set text desc
    let eViewtators = const f <$> click b 
    return (eViewtators,return b)

{-
An "IO" button (always total) can do is the 
same type as a UI button, but is allowed 
to do IO.
-}
ioButton :: String -> (AppState -> IO AppState) -> UI (Event [Mutator],UI Element)
ioButton desc f = do
    b <- UI.button # set text desc
    let eMutators = (const $ IOMutator f) <$> click b 
    return (fmap return eMutators,return b)

makeDefaultPanel :: String -> [UI (Event [Mutator],UI Element)] -> UI (Event [Mutator],UI Element)
makeDefaultPanel s items = do
    items' <- sequence items
    let eMutators = fmap concat $ unions $ map (^._1) items'
    p <- UI.div #. "panel"
    heading <- UI.div # set text s #. "panel-heading"
    contents <- UI.div #+ map (^._2) items'
    return p #+ [return heading, return contents #. "panel-contents"]
    return (eMutators,return p)

makeTrimPanel :: String -> [UI (Event [Mutator],UI Element)] -> UI (Event [Mutator],UI Element)
makeTrimPanel s items = do
    items' <- sequence items
    let eMutators = fmap concat $ unions $ map (^._1) items'
    p <- UI.div #. "panel" # set UI.id_ "trim"
    heading <- UI.div # set text s #. "panel-heading"
    contents <- UI.div #+ map (^._2) items'
    return p #+ [return heading, return contents #. "panel-contents"]
    return (eMutators,return p)

filterRow :: String -> (Comparison -> String -> AppState -> AppState) -> UI (Event [Mutator],UI Element)
filterRow desc f = do
    (eMutators,h) <- liftIO $ newEvent
    compareToInput <- UI.input

    let makeButton comp textcomp = do
        b <- UI.button # set text textcomp
        on click b $ const $ do
            compareToValue <- compareToInput # get value
            liftIO $ h (PureMutator $ f comp compareToValue)
        return b

    ble <- makeButton LE "<"
    bleq <- makeButton LEQ "<="
    bq <- makeButton Equal "=="
    bgeq <- makeButton GEQ ">="
    bge <- makeButton GE ">"

    let
        el = row    [ string desc
                    , return ble, return bleq, return bq, return bgeq, return bge
                    , return compareToInput
                    ]
    return (fmap return eMutators,el)

addRemoveRow :: String -> (AddRemove -> Music -> Music) -> UI (Event [Mutator],UI Element)
addRemoveRow obj f = do
    (eMutators,h) <- liftIO $ newEvent

    let makeButton addremove textcomp = do
        b <- UI.button # set text textcomp
        on click b $ const $ do
            liftIO $ h (PureMutator $ makeAction $ f addremove)
        return b

    addb <- makeButton Add ("add "++obj) 
    removeb <- makeButton Remove ("remove "++obj)
    
    let
        el = row [ return addb, return removeb ]
    return (fmap return eMutators,el)