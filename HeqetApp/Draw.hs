module HeqetApp.Draw 
    ( draw
    ) where

{-
Functions to draw a Heqet list of symbols to
an html5 canvas using the Threepenny-GUI interface.

-}

import HeqetApp.Types

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Canvas
import Control.Lens hiding ((#),set')

-- the header is the strip on the left of the canvas that
-- gives the current clef, key, staff number, instrument.
headerWidth = 150 :: PX

-- the number of staff units between each staff
staffSpacing = 15 :: Double

-- the number of staff units that the top
-- staff is below the top of the canvas
topStaffOffset = 14 :: Double

draw :: [HeadingSymbol] -> [Symbol] -> ViewState -> Canvas -> UI ()
draw hsyms syms viewstate canvas = do
    canvas # dot 1 0 0 viewstate
    canvas # staffLines viewstate
    return ()

calcX :: PointInTime -> ViewState -> PX
calcX pit viewstate = let
    timeFromStartOfView = pit - (viewstate^.startTime)
    distFromStartOfView = (fromRational timeFromStartOfView) * (viewstate^.timeScale)
    in headerWidth + distFromStartOfView

calcY :: StaffN -> StaffPosition -> ViewState -> PX
calcY staffN staffPos viewstate = let
    staffNFromTop = staffN - (viewstate^.topStaff)
    yStaffUnits = 
        (fromIntegral staffNFromTop) * staffSpacing
        + topStaffOffset
        + (fromIntegral staffPos)
    in yStaffUnits * (viewstate^.staffSize)

-- dot at a pixel position
dot' :: (PX,PX) -> Canvas -> UI ()
dot' point canvas = do
    canvas # beginPath
    canvas # arc point 2 0 7
    canvas # closePath
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # fill
    return ()

-- dot at a time/staff position
dot :: StaffN -> StaffPosition -> PointInTime -> ViewState -> Canvas -> UI ()
dot staffN staffPos pit viewstate canvas = let
    x = calcX pit viewstate
    y = calcY staffN staffPos viewstate
    in canvas # dot' (x,y)

staffLines :: ViewState -> Canvas -> UI ()
staffLines viewstate canvas = do
    let 
        startX = calcX (viewstate^.startTime) viewstate
        stopX = calcX (viewstate^.endTime) viewstate
        staffNs = [(viewstate^.topStaff)..(viewstate^.bottomStaff)]
        ys = map (\n -> calcY n 0 viewstate) staffNs
    mapM_ (\y -> lines5 (viewstate^.staffSize) startX stopX y canvas) ys

lines5 :: PX -> PX -> PX -> PX -> Canvas -> UI ()
lines5 spacing startX stopX y canvas = do
    let
        diffs = [4,2,0,-2,-4]
        pixelDiffs = map (*spacing) diffs
        pixelAbs = map (+y) pixelDiffs
    mapM_ (line1 startX stopX canvas) pixelAbs

line1 :: PX -> PX -> Canvas -> PX -> UI ()
line1 startX stopX canvas y = do
    canvas # beginPath
    canvas # moveTo (startX,y)
    canvas # lineTo (stopX,y)
    canvas # stroke