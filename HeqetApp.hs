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
import Safe
import Data.Ratio

import System.IO.Unsafe -- NOOOOO
import Control.Concurrent

main :: IO ()
main = do
    startGUI defaultConfig {jsStatic = return "static", jsCustomHTML = return "index.html" } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Heqet"
    -- we know canvasdiv and paneldiv exist because they're in start.html
    canvasdiv <- UI.div # set UI.id_ "canvasdiv"
    canvasWidthDiv <- UI.div # set UI.id_ "canvasWidthDiv"
    canvasHeightDiv <- UI.div # set UI.id_ "canvasHeightDiv"
    viewStartTime <- UI.div # set UI.id_ "viewStartTime"
    viewCursorTime <- UI.div # set UI.id_ "viewCursorTime"
    return canvasdiv #+ 
        [return canvasWidthDiv
        ,return canvasHeightDiv
        ,return viewStartTime
        ,return viewCursorTime
        ]
    paneldiv <- UI.div # set UI.id_ "paneldiv"
    canvas <- UI.canvas #. "musicspace"
    return canvasdiv #+ [return canvas]
    panels <- HeqetApp.Interface.panels
    navbar <- HeqetApp.Interface.navbar
    return paneldiv #+ [column $ (snd navbar) : map snd panels ]
    
    let
        eKeyboard = never :: Event [Mutator]
        eButtons = fmap concat $ unions $ map fst panels
        eClicks = never :: Event [Mutator]
        -- eActions has a () every time the user does something
        eActions = unions $ (const () <$> fst navbar):(map (const () <$>) [eKeyboard, eButtons, eClicks])
        unsafeKeyboard =  map makeUnsafeMutations <$> eKeyboard
        unsafeButtons  =  map makeUnsafeMutations <$> eButtons
        unsafeClicks   =  map makeUnsafeMutations <$> eClicks
        fss = unions
            [ unsafeKeyboard
            , unsafeButtons
            , unsafeClicks
            ]
        fs = concatenate.concat <$> fss
        
    (eCanvasSize, postCanvasSize) <- liftIO $ newEvent
    
    timer <- UI.timer # set UI.interval 250
    onEvent (UI.tick timer) $ \_ -> do
        w <- get value canvasWidthDiv
        h <- get value canvasHeightDiv
        liftIO $ postCanvasSize (readDef 500 w,readDef 500 h)
    UI.start timer
    let
        eNavbar = fst navbar
        navfss = unions [eNavbar, (return.recalculateCanvasSize) <$> eCanvasSize]
        navfs = concatenate.concat <$> navfss
    
    bAppState <- accumB startingAppState fs
    bViewState <- accumB defaultViewState navfs
    let bSumState = (,) <$> bAppState <*> bViewState
    
    return viewStartTime # sink text (betterRationalShow._startTime <$> bViewState)
    return viewCursorTime # sink text (pure "foo")
    
    -- Todo: listbox
    
    -- Todo: inspector
    
    -- Todo: history
    
    onEvent eActions $ const $ do
        liftIO $ threadDelay 400 -- sorry
        (appState,viewState) <- currentValue bSumState
        canvas # UI.clearCanvas
        let 
            (x,m) = appState
            (hsyms,syms) = Heqet.Output.Symbols.renderSymbols m () 0 
        canvas # HeqetApp.Draw.draw hsyms syms viewState

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

recalculateCanvasSize :: (PX,PX) -> ViewState -> ViewState
recalculateCanvasSize (x,y) = id

betterRationalShow :: (Integral a, Show a) => Ratio a -> String
betterRationalShow r = (show $ numerator r) ++ "/" ++ (show $ denominator r)