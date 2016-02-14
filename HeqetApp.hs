import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Events
import Data.IORef

type ApplicationState = Int

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
    applicationState <- liftIO $ newIORef 100
    return paneldiv #+ [column $ map ($ applicationState) [navigation, file, edit, notation, music, select, history]]
    getBody window #+ [row [return canvasdiv, return paneldiv]]
    UI.addStyleSheet window "main.css"
    
    on mousedown canvas $ \e -> do
        old <- liftIO $ readIORef applicationState
        liftIO $ writeIORef applicationState (100)
    
    timer <- UI.timer # set UI.interval 50
    redrawTick <- accumE (0::Int) $ (+1) <$ UI.tick timer
    onEvent redrawTick $ \stepNum -> do
        canvas # UI.clearCanvas
        canvas # set' UI.fillStyle (UI.htmlColor "black")
        l <- liftIO $ readIORef applicationState
        canvas # UI.fillRect (100,100) (fromIntegral l) 50

    UI.start timer
    return ()

musicspace :: UI Element
musicspace = UI.canvas #. "musicspace"

file, edit, notation, music, select, history :: (IORef ApplicationState) -> UI Element
navigation state = UI.div # set text "<<< << < > >> >>> up down (+) (-)"
file state = makeFooPanel state "file"
edit state = makeDefaultPanel state "edit" [((+50),"make longer")]
notation state = makeFooPanel state "notation"
music state = makeFooPanel state "music"
select state = makeFooPanel state "select"
history state = makeFooPanel state "history"

foo = UI.div # set text "fooasld;;;;;;;;;;fjkkljds flsklfs dkljlkdfs lsklsksdkj flksdjfa ;sjflaks dfioeqrgjnoeqr nbvo[qerbnoqen voq[envo[qerignvo qeirfjkkljdsflsklfsdkljlkdfslsklsksdkj flksdjf a;sjflaksd fioeqrgj noeqrnbvo[qerbn oqenvoq[envo[qerignvoqeirfjkkljd sflsklfs dkljlkdfslsklsk sdkjflksdjfa;sj flaksdfioeqrgjnoe qrnbvo[qerbnoqe  nvoq[en vo[qerignvoq eirfjkkl jdsflsklfsdkljlkdfslsklsksdkjflksdjfa;sjflaksdfioeqrgjnoeqrnbvo[qerbnoqenvoq[envo[qerignvoqeirfjkkljdsf lsklfsdkljl kdfslsklsks dkjflksdjfa;s jflaksdfioeq rgjnoeqrnbvo[ qerbnoqenvo q[envo[qer ignvoqeir"

makeFooPanel :: (IORef ApplicationState)  -> String -> UI Element
makeFooPanel _ s = do
    p <- UI.div #. "panel"
    heading <- UI.div # set text s #. "panel-heading"
    contents <- UI.div #+ [foo]
    empty <- UI.div
    element p #+ [return heading, return contents #. "panel-contents"]
    return p

makeDefaultPanel :: (IORef ApplicationState)  -> String -> [(ApplicationState -> ApplicationState, String)] -> UI Element
makeDefaultPanel state s fs = do
    p <- UI.div #. "panel"
    heading <- UI.div # set text s #. "panel-heading"
    contents <- UI.div #+ map (makeDefaultButton state) fs
    empty <- UI.div
    element p #+ [return heading, return contents #. "panel-contents"]
    return p

makeDefaultButton :: (IORef ApplicationState) -> (ApplicationState -> ApplicationState, String) -> UI Element
makeDefaultButton state (f,desc) = do
    b <- UI.button # set text desc
    on click b $ const $ do
        old <- liftIO $ readIORef state
        liftIO $ writeIORef state (f old)
    return b
{-
data HideShow a = HideShow 
    { hsHeader :: Element
    , hsContent :: a
    }

hideShow :: Element -> Widget -> IO (HideShow a)
hideShow e h = do
    
    return $ HideShow { elementHS = contents, elementHeader = header }

instance Widget (HideShow a) where getElement = elementHS






    let clicks = UI.click heading
        flips = const not <$> clicks
    showing <- accumB False flips
    showingcontents <- fmap (\b -> if b then contents else empty) showing
    
-}