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
    return paneldiv #+ [column $ map ($ state) HeqetApp.Interface.panels]
    getBody window #+ [row [return canvasdiv, return paneldiv]]
    UI.addStyleSheet window "main.css"
    
    on mousedown canvas $ \e -> do
        old <- liftIO $ readIORef state
        liftIO $ writeIORef state (100,())
    
    let viewstate = ViewState {
          _startTime = 0
        , _endTime = 10
        , _topStaff = 1
        , _staffSize = 5
        }
    
    timer <- UI.timer # set UI.interval 50
    redrawTick <- accumE (0::Int) $ (+1) <$ UI.tick timer
    onEvent redrawTick $ \stepNum -> do
        canvas # UI.clearCanvas
        canvas # set' UI.fillStyle (UI.htmlColor "black")
        (l,_) <- liftIO $ readIORef state
        canvas # UI.fillRect (100,100) (fromIntegral l) 50
        return $ HeqetApp.Draw.draw (return canvas) [] [] viewstate

    UI.start timer
    return ()
