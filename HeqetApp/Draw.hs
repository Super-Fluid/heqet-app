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
    mapM_ (\(s',sn,sp,pit) -> drawSymbol sp (placeAndScale sn sp pit viewstate) (*(viewstate^.staffSize)) canvas s') syms

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
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # fill
    return ()

-- dot at a time/staff position
dot :: StaffN -> StaffPosition -> PointInTime -> ViewState -> Canvas -> UI ()
dot staffN staffPos pit viewstate canvas = let
    x = calcX pit viewstate
    y = calcY staffN staffPos viewstate
    in canvas # dot' (x,y)

blackFill :: Canvas -> UI ()
blackFill canvas = canvas # set' UI.fillStyle (UI.htmlColor "red")

thinLine :: Canvas -> UI ()
thinLine canvas = canvas # set' strokeStyle "black" >> canvas # set' lineWidth 1 >> return ()

thickLine :: Canvas -> UI ()
thickLine canvas = canvas # set' strokeStyle "black" >> canvas # set' lineWidth 2 >> return ()

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
    absY = (-relY) * (viewstate^.staffSize) + originY
    in (absX,absY)

{- 
drawSymbol primarily draws the Symbol' on
the canvas, scaling and placing it using
the provided (Point -> Point) function
called f here. Some symbols need to know
where they are on the staff to know how to 
be drawn, so we also provide the symbol's
StaffPosition. If a symbol draws figures
like arcs which are measured in ways other
than just specifying points, we also can
scale them correctly with (PX -> PX) scale
function, which turns staff units to px.

-}
drawSymbol :: StaffPosition -> (Point -> Point) -> (PX -> PX) -> Canvas -> Symbol' -> UI ()
drawSymbol _ f _ canvas (NoteHead X) = do
    let 
        slash = map f [(0,-1),(2,1)]
        backslash = map f [(0,1),(2,-1)]
    canvas # thinLine
    canvas # drawPath slash
    canvas # drawPath backslash
drawSymbol _ f sc canvas (NoteHead Breve) = do
    canvas # thinLine
    let paths = (map.map) f [[(0,-1.5),(0,1.5)],[(0,0.75),(3,0.75)],[(0,-0.75),(3,-0.75)],[(3,-1.5),(3,1.5)]]
    canvas # drawFigure paths
drawSymbol _ f sc canvas (NoteHead Whole) = do
    canvas # thinLine
    let paths = (map.map) f [[(0,0),(1.5,1),(3,0),(1.5,-1)]]
    canvas # drawFigure paths
drawSymbol _ f sc canvas (NoteHead Half) = do
    canvas # thickLine
    canvas # beginPath
    canvas # arc (f(0,1)) (sc 1) 0 7
    canvas # stroke
drawSymbol _ f sc canvas (NoteHead Filled) = do
    canvas # blackFill
    canvas # beginPath
    canvas # arc (f(0,1)) (sc 1) 0 7
    canvas # fill
drawSymbol _ f _ canvas (Stem Up) = do
    let
        stem = map f [(2,0),(2,7)]
    canvas # thinLine
    canvas # drawPath stem
drawSymbol _ f _ canvas (Stem Down) = do
    let
        stem = map f [(0,0),(0,-7)]
    canvas # thinLine
    canvas # drawPath stem
drawSymbol _ f _ canvas (Flags Up n) = do
    let
        flag :: NumFlags -> UI ()
        flag x = do
            let path = map f [(2,7-(fromIntegral x)),(4,7-(fromIntegral x))]
            canvas # thickLine
            canvas # drawPath path
    mapM_ flag [0..(n-1)]
drawSymbol _ f _ canvas (Flags Down n) = do
    let
        flag :: NumFlags -> UI ()
        flag x = do
            let path = map f [(0,(fromIntegral x)-7),(2,(fromIntegral x)-7)]
            canvas # thickLine
            canvas # drawPath path
    mapM_ flag [0..(n-1)]
drawSymbol _ f _ canvas (Accidental DoubleFlat) = do
    let
        flat = map f [(-2,1),(-2,-1),(-1,0),(-2,0)]
    canvas # thinLine
    canvas # drawPath flat
drawSymbol _ f _ canvas (Accidental Flat) = do
    let
        flatflat = map f [(-2,1),(-2,-1),(-1,0),(-2,0),(-3,1),(-3,-1),(-2,0),(-3,0)]
    canvas # thinLine
    canvas # drawPath flatflat
drawSymbol _ f _ canvas (Accidental Natural) = do
    let
        natural = (map.map) f [[(-2,1),(-2,-0.5),(-1,0.5)],[(-2,0.5),(-1,0.5),(-1,-1)]]
    canvas # thinLine
    canvas # drawFigure natural
drawSymbol _ f _ canvas (Accidental Sharp) = do
    let
        sharp = (map.map) f [[(-2,-1),(-2,1)],[(-1,-1),(-1,1)],[(-2.5,0.5),(-0.5,0.5)],[(-2.5,-0.5),(-0.5,-0.5)]]
    canvas # thinLine
    canvas # drawFigure sharp
drawSymbol _ f _ canvas (Accidental DoubleSharp) = do
    let
        sharpsharp = (map.map) f [[(-2.5,-1),(-0.5,1)],[(-2.5,1),(-0.5,-1)]]
    canvas # thinLine
    canvas # drawFigure sharpsharp
drawSymbol _ f _ canvas (SimpleArticulation updn Marcato) =
    invertibleArticulation f canvas updn [[(0,0),(1,1),(2,0)]]
drawSymbol _ f _ canvas (SimpleArticulation updn Stopped) = 
    invertibleArticulation f canvas updn [[(0,0),(1,0),(1,1),(0,1),(0,0)]]
drawSymbol _ f _ canvas (SimpleArticulation updn Tenuto) =
    invertibleArticulation f canvas updn [[(0,0),(2,0)]]
drawSymbol _ f _ canvas (SimpleArticulation updn Staccatissimo) =
    invertibleArticulation f canvas updn [[(0,0),(0,2)]]
drawSymbol _ f _ canvas (SimpleArticulation updn Accent) =
    invertibleArticulation f canvas updn [[(0,0),(2,0.75),(0,1.5)]]
drawSymbol _ f sc canvas (SimpleArticulation updn Staccato) = do
    canvas # beginPath
    canvas # arc (f(0,1)) (sc 1) 0 7
    canvas # blackFill
    canvas # fill
drawSymbol _ f _ canvas (SimpleArticulation updn Portato) =
    invertibleArticulation f canvas updn [[(0,0),(2,0)],[(0,0.5),(2,1.25),(0,2)]]
drawSymbol _ f _ canvas (Markup s) = return ()
drawSymbol _ f _ canvas (TextDynamic s) = return ()
drawSymbol _ f _ canvas (TextMeter s) = return ()
drawSymbol _ f _ canvas (KeyChange n) = return ()
drawSymbol _ f _ canvas (Barline) = do
    let 
        bottom = f (0,-4)
        top = f (0,4)
    canvas # thinLine
    canvas # beginPath
    canvas # moveTo bottom
    canvas # lineTo top
    canvas # stroke 
drawSymbol _ f _ canvas (Clef Treble) = return ()
drawSymbol _ f _ canvas (Clef Alto) = return ()
drawSymbol _ f _ canvas (Clef Treble8) = return ()
drawSymbol _ f _ canvas (Clef Tenor) = return ()
drawSymbol _ f _ canvas (Clef Bass) = return ()
drawSymbol _ f _ canvas (Clef (CustomClef s)) = return ()
drawSymbol _ f sc canvas (Rest) = return ()
drawSymbol _ f _ canvas (Tie) = return ()
drawSymbol _ f _ canvas (Slur updown) = return ()
drawSymbol staffPos f sc canvas (LedgerLines) = return ()
drawSymbol _ f _ canvas (InsertionPoint) = return ()
drawSymbol _ f sc canvas (Color clr) = return ()
drawSymbol _ f sc canvas Selection = return ()

invertibleArticulation :: (Point -> Point) -> Canvas -> UpDown -> [[Point]] -> UI()
invertibleArticulation f canvas updn points = do
    let
        baseY = case updn of Up -> 8; Down -> -8
        inverter = case updn of Up -> 1; Down -> -1
        path = (map.map) (f.(\(x,y) -> (x,baseY+(y*inverter)))) points
    canvas # thinLine
    canvas # drawFigure path

drawPath :: [Point] -> Canvas -> UI ()
drawPath ps canvas = do
    canvas # beginPath
    mapM_ (\p -> canvas # moveTo p) ps
    canvas # stroke

drawFigure :: [[Point]] -> Canvas -> UI ()
drawFigure pss canvas = mapM_ (\ps -> drawPath ps canvas) pss

drawHeadingSymbol :: (Point -> Point) -> Canvas -> HeadingSymbol' -> UI ()
drawHeadingSymbol f canvas hs = return ()