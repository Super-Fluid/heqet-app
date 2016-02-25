import Control.Monad
import Control.Applicative

import HeqetApp.Types
import qualified HeqetApp.Draw
import qualified HeqetApp.Interface

import qualified Heqet
import Heqet.Types

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
    
    let defaultViewState = ViewState {
          _startTime = 0
        , _endTime = 10
        , _timeScale = 100
        , _topStaff = 1
        , _bottomStaff = 6
        , _staffSize = 5
        }
        eKeyboard = never :: Event Mutator
        eButtons = never :: Event Mutator
        eClicks = never :: Event Mutator
        input = unions [eKeyboard, eButtons, eClicks]
        fss = map doMutations <$> input
        fs = concatenate <$> fss
    
    appState <- accumE startingAppState fs
    
    (eCanvasSize, postCanvasSize) <- liftIO $ newEvent
    let
        eNavbar = never :: Event (ViewState -> ViewState)
    
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
        viewstate <- liftIO $ readIORef viewStateRef
        canvas # UI.clearCanvas
        canvas # HeqetApp.Draw.draw exampleHeading exampleSymbols viewstate
        -}
    {-
    timer <- UI.timer # set UI.interval 250
    redrawTick <- accumE (0::Int) $ (+1) <$ UI.tick timer
    onEvent redrawTick $ \stepNum -> do
    UI.start timer
    -}
    return ()

-- ARRGHGHGH
doMutations :: Mutator -> (AppState -> AppState)
doMutations (PureMutator f) = f
doMutations (IOMutator f) = unsafePerformIO . f