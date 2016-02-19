import Control.Monad

import HeqetApp.Types
import qualified HeqetApp.Draw
import qualified HeqetApp.Interface

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Events
import Data.IORef

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
    state <- liftIO $ newIORef (100,())
    extraState <- liftIO $ newIORef () --- ARGH
    return paneldiv #+ [column $
        (HeqetApp.Interface.navbar extraState)
        : map ($ state) HeqetApp.Interface.panels
        ]
    getBody window #+ [row [return canvasdiv, return paneldiv]]
    UI.addStyleSheet window "main.css"
    
    let viewstate = ViewState {
          _startTime = 0
        , _endTime = 10
        , _timeScale = 100
        , _topStaff = 1
        , _bottomStaff = 4
        , _staffSize = 5
        }
    
    on mousedown canvas $ \e -> do
        old <- liftIO $ readIORef state
        liftIO $ writeIORef state (100,())
        
        canvas # UI.clearCanvas
        canvas # HeqetApp.Draw.draw exampleHeading exampleSymbols viewstate
    {-
    timer <- UI.timer # set UI.interval 250
    redrawTick <- accumE (0::Int) $ (+1) <$ UI.tick timer
    onEvent redrawTick $ \stepNum -> do
    UI.start timer
    -}
    return ()
