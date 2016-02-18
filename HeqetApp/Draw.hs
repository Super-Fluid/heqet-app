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
headerWidth = 90 :: PX

-- the number of staff units between each staff
staffSpacing = 19 :: Double

-- the number of staff units that the top
-- staff is below the top of the canvas
topStaffOffset = 15 :: Double

draw :: [HeadingSymbol] -> [Symbol] -> ViewState -> Canvas -> UI ()
draw hsyms syms viewstate canvas = do
    canvas # dot 2 0 1 viewstate
    canvas # staffLines viewstate
    mapM_ (\(s',sn,sp,pit) -> drawSymbol sp (placeAndScale sn sp pit viewstate) canvas s') syms

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

{-
The symbols are specified relative to their
own origin. To draw a symbol, use placeAndScale
to make a function that converts them to absolute
coordinates, and pass it into the symbol-drawing
function.
-}
placeAndScale :: StaffN -> StaffPosition -> PointInTime -> ViewState -> (Point -> Point)
placeAndScale staffN staffPos pit viewstate (relX,relY) = let
    originX = calcX pit viewstate
    originY = calcY staffN staffPos viewstate
    absX = relX * (viewstate^.staffSize) + originX
    absY = relY * (viewstate^.staffSize) + originY
    in (absX,absY)

drawSymbol :: StaffPosition -> (Point -> Point) -> Canvas -> Symbol' -> UI ()
drawSymbol _ f canvas (NoteHead X) = return ()
drawSymbol _ f canvas (NoteHead Breve) = return ()
drawSymbol _ f canvas (NoteHead Whole) = return ()
drawSymbol _ f canvas (NoteHead Half) = return ()
drawSymbol _ f canvas (NoteHead Filled) = return ()
drawSymbol _ f canvas (Stem Up) = return ()
drawSymbol _ f canvas (Stem Down) = return ()
drawSymbol _ f canvas (Flags Up n) = return ()
drawSymbol _ f canvas (Flags Down n) = return ()
drawSymbol _ f canvas (Accidental DoubleFlat) = return ()
drawSymbol _ f canvas (Accidental Flat) = return ()
drawSymbol _ f canvas (Accidental Natural) = return ()
drawSymbol _ f canvas (Accidental Sharp) = return ()
drawSymbol _ f canvas (Accidental DoubleSharp) = return ()
drawSymbol _ f canvas (SimpleArticulation updn Marcato) = return ()
drawSymbol _ f canvas (SimpleArticulation updn Stopped) = return ()
drawSymbol _ f canvas (SimpleArticulation updn Tenuto) = return ()
drawSymbol _ f canvas (SimpleArticulation updn Staccatissimo) = return ()
drawSymbol _ f canvas (SimpleArticulation updn Accent) = return ()
drawSymbol _ f canvas (SimpleArticulation updn Staccato) = return ()
drawSymbol _ f canvas (SimpleArticulation updn Portato) = return ()
drawSymbol _ f canvas (Markup s) = return ()
drawSymbol _ f canvas (TextDynamic s) = return ()
drawSymbol _ f canvas (TextMeter s) = return ()
drawSymbol _ f canvas (KeyChange n) = return ()
drawSymbol _ f canvas (Barline) = return ()
drawSymbol _ f canvas (Clef Treble) = return ()
drawSymbol _ f canvas (Clef Alto) = return ()
drawSymbol _ f canvas (Clef Treble8) = return ()
drawSymbol _ f canvas (Clef Tenor) = return ()
drawSymbol _ f canvas (Clef Bass) = return ()
drawSymbol _ f canvas (Clef (CustomClef s)) = return ()
drawSymbol _ f canvas (Rest) = return ()
drawSymbol _ f canvas (Tie) = return ()
drawSymbol _ f canvas (Slur updown) = return ()
drawSymbol staffPos f canvas (LedgerLines) = return ()



drawHeadingSymbol :: (Point -> Point) -> Canvas -> HeadingSymbol' -> UI ()
drawHeadingSymbol f canvas hs = return ()