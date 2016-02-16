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
An "IO" button (always total) can do io, like
saving to a file.
-}
ioButton :: String -> (AppState -> IO AppState) -> IORef AppState -> UI Element
ioButton desc f state = do
    b <- UI.button # set text desc
    on click b $ const $ do
        old <- liftIO $ readIORef state
        new <- liftIO $ f old
        liftIO $ writeIORef state new
    return b