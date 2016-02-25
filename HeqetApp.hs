import Control.Monad
import Control.Applicative

import HeqetApp.Types
import qualified HeqetApp.Draw
import qualified HeqetApp.Interface

import qualified Heqet
import Heqet.Types
import qualified Heqet.Output.Symbols

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Events
import Reactive.Threepenny
import Data.IORef

import System.IO.Unsafe -- NOOOOO

main :: IO ()
main = do
    startGUI defaultConfig {jsStatic = return "static", jsCustomHTML = return "index.html" } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Heqet"
    -- we know canvasdiv and paneldiv exist because they're in start.html
    canvasdiv <- UI.div # set UI.id_ "canvasdiv"
    paneldiv <- UI.div # set UI.id_ "paneldiv"
    canvas <- UI.canvas #. "musicspace"
    return canvasdiv #+ [return canvas]
    
    let
        eKeyboard = never :: Event Mutator
        eButtons = never :: Event Mutator
        eClicks = never :: Event Mutator
        input = unions [eKeyboard, eButtons, eClicks]
        fss = map makeUnsafeMutations <$> input
        fs = concatenate <$> fss
        
    (eCanvasSize, postCanvasSize) <- liftIO $ newEvent
    
    timer <- UI.timer # set UI.interval 250
    onEvent (UI.tick timer) $ \_ -> do
        liftIO $ postCanvasSize (500,500)
    UI.start timer
    let
        eNavbar = never :: Event (ViewState -> ViewState)
        navfss = unions [eNavbar, reviseNavBar <$> eCanvasSize]
        navfs = concatenate <$> navfss
    
    bAppState <- accumB startingAppState fs
    bViewState <- accumB defaultViewState navfs
    let bSumState = (,) <$> bAppState <*> bViewState
    let eSumState = bSumState <@ input
    
    -- Todo: listbox
    
    -- Todo: inspector
    
    -- Todo: history
    
    onEvent eSumState $ \(appState,viewState) -> do
        canvas # UI.clearCanvas
        let 
            (x,m) = appState
            (hsyms,syms) = Heqet.Output.Symbols.renderSymbols m () 0 
        canvas # HeqetApp.Draw.draw hsyms syms viewState

        
{-        
    return paneldiv #+ [column $
        (HeqetApp.Interface.navbar viewStateRef)
        : map ($ state) HeqetApp.Interface.panels
        ]
        -}
    getBody window #+ [row [return canvasdiv, return paneldiv]]
    UI.addStyleSheet window "main.css"
    
    {-
    on mousedown canvas $ \e -> do
        old <- liftIO $ readIORef state
        liftIO $ writeIORef state (100,())
    -}
    return ()

-- ARRGHGHGH
makeUnsafeMutations :: Mutator -> (AppState -> AppState)
makeUnsafeMutations (PureMutator f) = f
makeUnsafeMutations (IOMutator f) = unsafePerformIO . f

reviseNavBar :: (PX,PX) -> ViewState -> ViewState
reviseNavBar (x,y) = id