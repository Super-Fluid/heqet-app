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
import Data.Maybe

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
    canvas # staffLines viewstate
    let visibleSyms = filter (\(_,sn,sp,pit) -> pit >= viewstate^.startTime && pit <= viewstate^.endTime && sn >= viewstate^.topStaff && sn <= viewstate^.bottomStaff) syms
    mapM_ (\(s',sn,sp,pit) -> drawSymbol sp (placeAndScale sn sp pit viewstate) (*(viewstate^.staffSize)) canvas s') visibleSyms
    mapM_ (\(hs',sn) -> drawHeadingSymbol 0 (placeAndScaleH sn viewstate) (*(viewstate^.staffSize)) canvas hs') hsyms

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
        - (fromIntegral staffPos)
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
blackFill canvas = canvas # set' UI.fillStyle (UI.htmlColor "black")

thinLine :: (PX -> PX) -> Canvas -> UI ()
thinLine sc canvas = canvas # set' strokeStyle "black" >> canvas # set' lineWidth (sc 0.2) >> return ()

thickLine :: (PX -> PX) -> Canvas -> UI ()
thickLine sc canvas = canvas # set' strokeStyle "black" >> canvas # set' lineWidth (sc 0.52) >> return ()

textFill :: Canvas -> UI ()
textFill canvas = do
    canvas # blackFill

colorTable :: [(HeqetApp.Types.Color,FillStyle)]
colorTable = 
    [ (DarkGreen, "#008800")
    , (Brown, "#774400")
    , (DarkBlue, "#0000ff")
    , (Red, "#ff0000")
    , (LightBlue, "#88ccff")
    , (Purple, "#8800cc")
    , (Yellow, "#ffff00")
    , (Orange, "#ff8800")
    , (Pink, "#ffaaaa")
    , (Grey, "#aaaaaa")
    ] & traverse._2 %~ htmlColor

colorCoordinates :: [(HeqetApp.Types.Color,[Point])]
colorCoordinates = 
    [ (DarkGreen, [(-2,-1.5),(-0.5,-3)])
    , (Brown, [(-2,0),(-2,1.5)])
    , (DarkBlue, [(4,-1.5),(2.5,-3)])
    , (Red, [(-2,0),(-2,-1.5)])
    , (LightBlue, [(-2,1.5),(-0.5,3)])
    , (Purple, [(4,0),(4,1.5)])
    , (Yellow, [(4,0),(4,-1.5)])
    , (Orange, [(1,3),(-0.5,3)])
    , (Pink, [(4,1.5),(2.5,3)])
    , (Grey, [(1,-3),(2.5,-3)])
    ] & traverse._2 %~ (\[a,b] -> [(1,0),a,b,(1,0)])

staffLines :: ViewState -> Canvas -> UI ()
staffLines viewstate canvas = do
    let 
        startX = calcX (viewstate^.startTime) viewstate
        stopX = calcX (viewstate^.endTime) viewstate
        staffNs = [(viewstate^.topStaff)..(viewstate^.bottomStaff)]
        ys = map (\n -> calcY n 0 viewstate) staffNs
    canvas # thinLine (*(viewstate^.staffSize))
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

placeAndScaleH :: StaffN -> ViewState -> (Point -> Point)
placeAndScaleH staffN viewstate (relX,relY) = let
    originX = calcX (viewstate^.startTime) viewstate
    originY = calcY staffN 0 viewstate
    absX = relX * (viewstate^.staffSize) + originX - 30
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
drawSymbol _ f sc canvas (NoteHead X) = do
    let 
        slash = map f [(0,-1),(2,1)]
        backslash = map f [(0,1),(2,-1)]
    canvas # thinLine sc
    canvas # drawPath slash
    canvas # drawPath backslash
drawSymbol _ f sc canvas (NoteHead Breve) = do
    canvas # thinLine sc
    let paths = (map.map) f [[(0,-1.5),(0,1.5)],[(0,0.75),(3,0.75)],[(0,-0.75),(3,-0.75)],[(3,-1.5),(3,1.5)]]
    canvas # drawFigure paths
drawSymbol _ f sc canvas (NoteHead Whole) = do
    canvas # thinLine sc
    let paths = (map.map) f [[(0,0),(1.5,1),(3,0),(1.5,-1),(0,0)]]
    canvas # drawFigure paths
drawSymbol _ f sc canvas (NoteHead Half) = do
    canvas # thinLine sc
    let paths = (map.map) f [[(0,0),(1,1),(2,0),(1,-1),(0,0)]]
    canvas # drawFigure paths
drawSymbol _ f sc canvas (NoteHead Filled) = do
    canvas # blackFill
    canvas # beginPath
    let points = map f [(0,0),(1,1),(2,0),(1,-1),(0,0)]
    mapM_ (\p -> canvas # lineTo p) points
    canvas # fill
drawSymbol _ f sc canvas (Stem Up) = do
    let
        stem = map f [(2,0),(2,7)]
    canvas # thinLine sc
    canvas # drawPath stem
drawSymbol _ f sc canvas (Stem Down) = do
    let
        stem = map f [(0,0),(0,-7)]
    canvas # thinLine sc
    canvas # drawPath stem
drawSymbol _ f sc canvas (Flags Up n) = do
    let
        flag :: NumFlags -> UI ()
        flag x = do
            let path = map f [(2,7-(fromIntegral x)),(4,7-(fromIntegral x))]
            canvas # thickLine sc
            canvas # drawPath path
    mapM_ flag [0..(n-1)]
drawSymbol _ f sc canvas (Flags Down n) = do
    let
        flag :: NumFlags -> UI ()
        flag x = do
            let path = map f [(0,(fromIntegral x)-7),(2,(fromIntegral x)-7)]
            canvas # thickLine sc
            canvas # drawPath path
    mapM_ flag [0..(n-1)]
drawSymbol _ f sc canvas (Accidental Flat) = do
    let
        flat = map f [(-2,2),(-2,-1),(-1,0.6),(-2,0.6)]
    canvas # thinLine sc
    canvas # drawPath flat
drawSymbol _ f sc canvas (Accidental DoubleFlat) = do
    let
        flatflat = map f [(-2,2),(-2,-1),(-1,0.6),(-2,0.6),(-3,0.6),(-3,2),(-3,-1),(-2,0.6),(-3,0.6)]
    canvas # thinLine sc
    canvas # drawPath flatflat
drawSymbol _ f sc canvas (Accidental Natural) = do
    let
        natural = (map.map) f [[(-2,2),(-2,-0.5),(-1,-0.5)],[(-2,0.5),(-1,0.5),(-1,-2)]]
    canvas # thinLine sc
    canvas # drawFigure natural
drawSymbol _ f sc canvas (Accidental Sharp) = do
    let
        sharp = (map.map) f [[(-2,-2),(-2,1.8)],[(-1,-1.8),(-1,2)],[(-2.5,0.5),(-0.5,0.8)],[(-2.5,-0.8),(-0.5,-0.5)]]
    canvas # thinLine sc
    canvas # drawFigure sharp
drawSymbol _ f sc canvas (Accidental DoubleSharp) = do
    let
        sharpsharp = (map.map) f [[(-2.5,-1),(-0.5,1)],[(-2.5,1),(-0.5,-1)]]
    canvas # thinLine sc
    canvas # drawFigure sharpsharp
drawSymbol _ f sc canvas (SimpleArticulation updn Marcato) =
    invertibleArticulation f sc canvas updn [[(0,0),(1,1),(2,0)]]
drawSymbol _ f sc canvas (SimpleArticulation updn Stopped) = 
    invertibleArticulation f sc canvas updn [[(0,0),(1,0),(1,1),(0,1),(0,0)]]
drawSymbol _ f sc canvas (SimpleArticulation updn Tenuto) =
    invertibleArticulation f sc canvas updn [[(0,0),(2,0)]]
drawSymbol _ f sc canvas (SimpleArticulation updn Staccatissimo) =
    invertibleArticulation f sc canvas updn [[(0,0),(0,2)]]
drawSymbol _ f sc canvas (SimpleArticulation updn Accent) =
    invertibleArticulation f sc canvas updn [[(0,0),(2,0.75),(0,1.5)]]
drawSymbol _ f sc canvas (SimpleArticulation updn Staccato) =
    invertibleArticulation f sc canvas updn [[(1,0),(1,0.5)]]
drawSymbol _ f sc canvas (SimpleArticulation updn Portato) =
    invertibleArticulation f sc canvas updn [[(0,0),(2,0)],[(0,0.5),(2,1.25),(0,2)]]
drawSymbol _ f sc canvas (Markup s) = do
    canvas # textFill
    canvas # fillText s (f(0,10))
drawSymbol _ f sc canvas (TextDynamic s) = do
    canvas # textFill
    canvas # fillText s (f(0,-10))
drawSymbol _ f sc canvas (TextMeter s) = do
    canvas # textFill
    canvas # fillText s (f(0,0))
drawSymbol _ f sc canvas (KeyChange n) = do
    canvas # textFill
    let s = if n>0 
        then show n ++ "#"
        else if n<0 
            then show (-n) ++ "b"
            else "0"
    canvas # fillText s (f(0,4))
drawSymbol _ f sc canvas (Barline) = do
    let 
        bottom = f (0,-4)
        top = f (0,4)
    canvas # thinLine sc
    canvas # beginPath
    canvas # moveTo bottom
    canvas # lineTo top
    canvas # stroke 
drawSymbol _ f sc canvas (Clef Treble) = do
    let
        points = [(0,0),(-2,-2),(-4,0),(0,4),(-2,6),(-2,-4)] :: [Point]
        path = map f (points & traverse._2 %~ (subtract 2))
    canvas # thinLine sc
    canvas # drawPath path
drawSymbol _ f sc canvas (Clef Alto) = do
    let
        figure = (map.map) f [[(-3,-4),(-3,4)],[(-2,-4),(0,-2),(-2,0),(0,2),(-2,4)]]
    canvas # thinLine sc
    canvas # drawFigure figure
drawSymbol _ f sc canvas (Clef Treble8) = do
    let
        points = [[(0,0),(-2,-2),(-4,0),(0,4),(-2,6),(-2,-4)]]  :: [[Point]]
        figure = (map.map) f (points & traverse.traverse._2 %~ (subtract 2))
    canvas # thinLine sc
    canvas # drawFigure figure
    canvas # textFill
    canvas # fillText "8" (f(-2,-6))
drawSymbol _ f sc canvas (Clef Tenor) = do
    let
        points = [[(-3,-4),(-3,4)],[(-2,-4),(0,-2),(-2,0),(0,2),(-2,4)]] :: [[Point]]
        figure = (map.map) f (points & traverse.traverse._2 %~ (+2))
    canvas # thinLine sc
    canvas # drawFigure figure
drawSymbol _ f sc canvas (Clef Bass) = do
    let
        points = [(-2,-5),(0,-1),(0,1),(-2,1.5),(-2,-1)] :: [Point]
        path = map f (points & traverse._2 %~ (+2))
    canvas # thinLine sc
    canvas # drawPath path
drawSymbol _ f sc canvas (Clef (CustomClef s)) = do
    canvas # textFill
    canvas # fillText s (f(-3,0))
drawSymbol _ f sc canvas (Rest RBreve) = do
    canvas # blackFill
    canvas # beginPath
    let points = map f [(0,0),(1,0),(1,2),(0,2),(0,0)]
    mapM_ (\p -> canvas # lineTo p) points
    canvas # fill
drawSymbol _ f sc canvas (Rest R1) = do
    canvas # blackFill
    canvas # beginPath
    let points = map f [(0,1),(2,1),(2,2),(0,2),(0,1)]
    mapM_ (\p -> canvas # lineTo p) points
    canvas # fill
drawSymbol _ f sc canvas (Rest R2) = do
    canvas # blackFill
    canvas # beginPath
    let points = map f [(0,0),(2,0),(2,1),(0,1),(0,0)]
    mapM_ (\p -> canvas # lineTo p) points
    canvas # fill
drawSymbol _ f sc canvas (Rest R4) = do
    let
        path = map f [(0,3),(1,2),(0,1),(1,0),(0,-1),(1,-2)]
    canvas # thinLine sc
    canvas # drawPath path
drawSymbol _ f sc canvas (Rest R8) = do
    restStem f sc canvas 1
    restFlags f sc canvas 1
drawSymbol _ f sc canvas (Rest R16) = do
    restStem f sc canvas 2
    restFlags f sc canvas 2
drawSymbol _ f sc canvas (Rest R32) = do
    restStem f sc canvas 3
    restFlags f sc canvas 3
drawSymbol _ f sc canvas (Rest R64) = do
    restStem f sc canvas 4
    restFlags f sc canvas 4
drawSymbol _ f sc canvas (Rest R128) = do
    restStem f sc canvas 5
    restFlags f sc canvas 5
drawSymbol _ f sc canvas (Dotting n) = return ()
drawSymbol _ f sc canvas (Tie) = return ()
drawSymbol _ f sc canvas (Slur updown) = return ()
drawSymbol staffPos f sc canvas (LedgerLines) = do
    let
        extra1 = if (staffPos `mod` 2 /= 0) then 1 else 0
        ys = if staffPos <= -6
             then map (+extra1) [0,2..(-6-staffPos)]
             else if staffPos >= 6
                  then map (subtract extra1) [0,-2..(6-staffPos)]
                  else []
    canvas # thinLine sc
    mapM_ (ledgerLine f canvas) ys
drawSymbol _ f sc canvas (InsertionPoint) = return ()
drawSymbol _ f sc canvas (Color clr) = do
    let fillstyle = fromJust $ lookup clr colorTable
    canvas # set' UI.fillStyle fillstyle
    canvas # beginPath
    let points = fromJust $ lookup clr colorCoordinates
    mapM_ (\p -> canvas # lineTo p) (map f points)
    canvas # fill
drawSymbol _ f sc canvas Selection = do
    canvas # set' strokeStyle "#0f0"
    canvas # set' lineWidth 3
    let path = (map.map) f [[(-1,-1),(0,-2),(2,-2),(3,-1),(3,1),(2,2),(0,2),(-1,1),(-1,-1)]]
    canvas # drawFigure path
drawSymbol _ f sc canvas (TextArticulation s) = return ()

ledgerLine :: (Point -> Point) -> Canvas -> Int -> UI()
ledgerLine f canvas y = canvas # drawPath (map f [(-0.5,(fromIntegral y)),(3,(fromIntegral y))])

restStem :: (Point -> Point) -> (PX -> PX) ->  Canvas -> NumFlags -> UI()
restStem f sc canvas n = do
    let 
        path = map f [(1,(fromIntegral n)+1),(1,-1)]
    canvas # thinLine sc
    canvas # drawPath path

restFlags :: (Point -> Point) -> (PX -> PX) ->  Canvas -> NumFlags -> UI()
restFlags f sc canvas n = do
    let
        points = [(1,0),(0,0.5),(0,0)]
        shiftBy y ps = ps & traverse._2 %~ (+(fromIntegral y))
        paths = (map.map) f $ map (\y -> shiftBy y points) [1..n]
    canvas # thinLine sc
    canvas # drawFigure paths

invertibleArticulation :: (Point -> Point) -> (PX -> PX) ->  Canvas -> UpDown -> [[Point]] -> UI()
invertibleArticulation f sc canvas updn points = do
    let
        baseY = case updn of Up -> 8; Down -> -8
        inverter = case updn of Up -> 1; Down -> -1
        path = (map.map) (f.(\(x,y) -> (x,baseY+(y*inverter)))) points
    canvas # thinLine sc
    canvas # drawFigure path

drawPath :: [Point] -> Canvas -> UI ()
drawPath ps canvas = do
    canvas # beginPath
    mapM_ (\p -> canvas # lineTo p) ps
    canvas # stroke

drawFigure :: [[Point]] -> Canvas -> UI ()
drawFigure pss canvas = mapM_ (\ps -> drawPath ps canvas) pss

drawHeadingSymbol :: StaffPosition -> (Point -> Point) -> (PX -> PX) -> Canvas -> HeadingSymbol' -> UI ()
drawHeadingSymbol pos f sc canvas (ClefH c) = drawSymbol pos f sc canvas (Clef c)
drawHeadingSymbol pos f sc canvas (KeyH n) = drawSymbol pos f sc canvas (KeyChange n)
drawHeadingSymbol _ f sc canvas (TextMeterH s) = return ()
drawHeadingSymbol _ f sc canvas (TopStaffBracket) = return ()
drawHeadingSymbol _ f sc canvas (BottomStaffBracket) = return ()
drawHeadingSymbol _ f sc canvas (InstName s) = return ()