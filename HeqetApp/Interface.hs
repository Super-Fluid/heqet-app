module HeqetApp.Interface 
    (panels
    ,navbar
    ) where

import HeqetApp.Types
import HeqetApp.Buttons
import qualified HeqetApp.Interface.Navbar as Nav

import Heqet.Types

import HeqetApp.Functions

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Events
import Control.Lens hiding ((#),view,set)

navbar :: UI (Event [(ViewState -> ViewState)], UI Element)
navbar  = do
    let items = 
            [ viewButton "0"   (Nav.toStart   ) 
            , viewButton "<<"  (Nav.fastbackward ) 
            , viewButton "<"   (Nav.backward   ) 
            , viewButton ">"   (Nav.forward    ) 
            , viewButton ">>"  (Nav.fastforward ) 
            , viewButton "^"   (Nav.up         ) 
            , viewButton "v"   (Nav.down       ) 
            , viewButton "<>" (Nav.stretch    ) 
            , viewButton "><" (Nav.squeeze    ) 
            , viewButton "+"   (Nav.larger     ) 
            , viewButton "-"   (Nav.smaller    ) 
            ]
    items' <- sequence items
    let el = row $ map (^._2) items'
    let eViewtators = unions $ map (^._1) items'
    return (eViewtators,el)
    

panels :: UI [(Event [Mutator],UI Element)]
panels = sequence
    [ file
    , edit
    , select
    , label
    , view

    , note
    , accidental
    , durationpanel
    , dynamics
    , articulation 
    , ties  
    , instrument
    , keypanel
    , clefpanel
    , meter
    , fingering 
    , lilypond
    , marks

    , transpose
    , functions
    , automation
    , graph
    , trim
    ]

file :: UI (Event [Mutator],UI Element)
file = makeDefaultPanel "File"
    [ return (never, heqetpath)
    , ioButton "Open" return
    , ioButton "Save" return
    , ioButton "Embed" return
    , return (never, lypath)
    , ioButton "Export Lilypond code" return
    , return (never,  row [string "concert score", UI.input # set UI.type_ "checkbox" # set UI.id_ "concertScore"] )
    , return (never,  row [string "transposed score", UI.input # set UI.type_ "checkbox" # set UI.id_ "transposedScore"] )
    , return (never,  row [string "parts", UI.input # set UI.type_ "checkbox" # set UI.id_ "includeParts"] )
    ]
    where 
        heqetpath = UI.input
        lypath = UI.input

edit :: UI (Event [Mutator],UI Element)
edit = do
    let
        ostinatoUntilInput = UI.input
    
    createOstinatoButton <- totalButton "create ostinato until" id
    
    makeDefaultPanel "Edit"
        [ totalButton "copy" id
        , totalButton "paste" id
        , totalButton "paste without history" id
        , totalButton "paste overlapping" id
        , return (never,  string "paste only:" )
        , totalButton "articulation" id
        , totalButton "dynamics" id
        , totalButton "meter" id
        , totalButton "key" id
        , totalButton "clef" id
        , totalButton "color" id
        , totalButton "string labels" id
        , totalButton "accidentals" id
        , totalButton "instrument" id
        , totalButton "staff" id
        , totalButton "substaff" id
        , totalButton "fingering" id
        , totalButton "markup" id
        , totalButton "lilypond" id
        , simpleButton "delete" id
        , totalButton "copy JSON" id
        , totalButton "paste JSON" id
        , return (createOstinatoButton^._1,  row [createOstinatoButton^._2 , ostinatoUntilInput] )
        ]

articulation :: UI (Event [Mutator],UI Element)
articulation = makeDefaultPanel "Articulation"
    [ simpleButton "remove all" id
    , addRemoveRow "slur" (const id)
    , addRemoveRow "staccato" (const id)
    , addRemoveRow "tenuto" (const id)
    , addRemoveRow "marcato" (const id)
    , addRemoveRow "accent" (const id)
    , addRemoveRow "portato" (const id)
    , addRemoveRow "staccatissimo" (const id)
    , addRemoveRow "stopped" (const id)
    ]

fingering :: UI (Event [Mutator],UI Element)
fingering = makeDefaultPanel "Fingering"
    [ simpleButton "remove" id
    , simpleButton "0" id
    , simpleButton "1" id
    , simpleButton "2" id
    , simpleButton "3" id
    , simpleButton "4" id
    , simpleButton "5" id
    , simpleButton "6" id
    , simpleButton "7" id
    , simpleButton "8" id
    , simpleButton "9" id
    , simpleButton "thumb" id
    ]

dynamics :: UI (Event [Mutator],UI Element)
dynamics = makeDefaultPanel "Dynamics"
    [ simpleButton "remove" id
    , simpleButton "fff" id
    , simpleButton "ff" id
    , simpleButton "f" id
    , simpleButton "mf" id
    , simpleButton "mp" id
    , simpleButton "p" id
    , simpleButton "pp" id
    , simpleButton "ppp" id
    , simpleButton "create cresc." id
    , simpleButton "create decresc." id
    , return (never,  string "other dynamic marks:" )
    , simpleButton "fp" id
    , simpleButton "sfz" id
    ]

instrument :: UI (Event [Mutator],UI Element)
instrument = makeDefaultPanel "Instrument"
    [ simpleButton "assign standard instrument" id
    , return (never,  UI.input )
    , simpleButton "remove" id
    , return (never,  string "info on instrument" )
    ]

keypanel :: UI (Event [Mutator],UI Element)
keypanel = makeDefaultPanel "Key"
    [ simpleButton "auto" id
    , simpleButton "remove" id
    , simpleButton "major" id
    , simpleButton "minor" id
    , simpleButton "C" id
    , simpleButton "Db" id
    , simpleButton "D" id
    , simpleButton "Eb" id
    , simpleButton "E" id
    , simpleButton "F" id
    , simpleButton "Gb" id
    , simpleButton "G" id
    , simpleButton "Ab" id
    , simpleButton "A" id
    , simpleButton "Bb" id
    , simpleButton "B" id
    ]

clefpanel :: UI (Event [Mutator],UI Element)
clefpanel = makeDefaultPanel "Clef"
    [ simpleButton "auto" id
    , simpleButton "remove" id
    , simpleButton "treble" id
    , simpleButton "alto" id
    , simpleButton "treble_8" id
    , simpleButton "tenor" id
    , simpleButton "bass" id
    , return (never,  string "manual clefs are only apparent in transposed view!" )
    ]

view :: UI (Event [Mutator],UI Element)
view = makeDefaultPanel "View"
    [ return (never,  UI.button # set text "concert" )
    , return (never,  UI.button # set text "transposed" )
    ]

select :: UI (Event [Mutator],UI Element)
select = do
    let
        insertionPointInput = UI.input
        staffEqInput = UI.input
        instEqInput = UI.input
        hasLilypondInput = UI.input
        measuresAtTimeInput = UI.input
        
        staffEqButton = totalButton "staff ==" id
        instEqButton = totalButton "instrument ==" id
        hasLilypondButton = totalButton "has lilypond:" id
        measuresAtTimeButton = totalButton "measures at time:" id
    
    xInsertionPoint <- insertionPointInput
    xStaffEq <- staffEqButton
    xInstEq <- instEqButton
    xHasLilypond <- hasLilypondButton
    xMeasuresAtTime <- measuresAtTimeButton
    
    makeDefaultPanel "Select"
        [ totalButton "all" id
        , totalButton "none" id
        , totalButton "invert" id
        , return (never,  row [ string "insertion point:", insertionPointInput ] )
        , return (never,  string "refine selection by:" )
        , filterRow "start time" (const $ const $ id)
        , filterRow "end time" (const $ const $ id)
        , filterRow "duration" (const $ const $ id)
        , filterRow "pitch" (const $ const $ id)
        , filterRow "dynamic" (const $ const $ id)
        , filterRow "time from measure start" (const $ const $ id)
        , return (fst xStaffEq,  row [ staffEqButton >>= snd , staffEqInput ] )
        , return (fst xInstEq,  row [ instEqButton >>= snd , instEqInput ] )
        , return (fst xHasLilypond,  row [ hasLilypondButton >>= snd , hasLilypondInput ] )
        , return (fst xMeasuresAtTime,  row [ measuresAtTimeButton >>= snd , measuresAtTimeInput ] )
        , totalButton "highest voice on staff" id
        , totalButton "middle voices on staff" id
        , totalButton "lowest voice on staff" id
        , totalButton "highest voice on substaff" id
        , totalButton "middle voices on substaff" id
        , totalButton "lowest voice on substaff" id
        , totalButton "is note" id
        , totalButton "is rest" id
        , totalButton "is percussion" id
        , totalButton "is effect" id
        , totalButton "is lyric" id
        , return (never,  string "expand selection by:" )
        , return (never,  row [string "consider notes individually", UI.input # set UI.type_ "checkbox" # set UI.id_ "expandByEachNote"] )
        , totalButton "hold time, relax staff" id
        , totalButton "hold time, relax substaff" id
        , totalButton "hold time and staff, relax voice" id
        , totalButton "hold time and substaff, relax voice" id
        , totalButton "same rhythm" id
        , totalButton "same rhythm on staff" id
        , totalButton "same rhythm on substaff" id
        , totalButton "same instrument" id
        ]

label :: UI (Event [Mutator],UI Element)
label = do
    colorers <- sequence $ map colorRow colorClasses
    let colorMutators = fmap concat $ unions $ map (^._1) colorers
    let colorButtons = map (^._2) colorers
    makeDefaultPanel "Label"
        [ return (colorMutators,  column colorButtons)
        , simpleButton "remove all colors" id
        , return (never,  string "string labels:" )
        , totalButton "select" id
        , totalButton "deselect" id
        , simpleButton "add" id
        , simpleButton "remove" id
        , return (never,  UI.input # set UI.id_ "string-label" )
        , simpleButton "remove all string labels" id
        ]

colorClasses = 
    [("darkgreen",DarkGreen)
    ,("brown",Brown)
    ,("darkblue",DarkBlue)
    ,("red",Red)
    ,("lightblue",LightBlue)
    ,("purple",Purple)
    ,("yellow",Yellow)
    ,("orange",Orange)
    ,("pink",Pink)
    ,("grey",Grey)
    ]

colorRow :: (String,Color) -> UI (Event [Mutator],UI Element)
colorRow (bid,color) = do
    sel <- colorButton "select" bid id
    desel <- colorButton "deselect" bid id
    clr <- colorButton "color" bid id
    unclr <- colorButton "uncolor" bid id
    let items = [sel, desel, clr, unclr]
    return (fmap concat $ unions $ map (^._1) items,row $ map (^._2) items)

transpose :: UI (Event [Mutator],UI Element)
transpose = makeDefaultPanel "Transpose"
    [ simpleButton "oct up" id
    , simpleButton "oct down" id
    , return (never,  string "by halfsteps:" )
    , simpleButton "+1 H" id
    , simpleButton "+2 W" id
    , simpleButton "+3 m3" id
    , simpleButton "+4 M3" id
    , simpleButton "+5 4" id
    , simpleButton "+6 tritone" id
    , simpleButton "+7 5" id
    , simpleButton "+8 m6" id
    , simpleButton "+9 M6" id
    , simpleButton "+10 m7" id
    , simpleButton "+11 M7" id
    , simpleButton "-1 H" id
    , simpleButton "2 W" id
    , simpleButton "-3 m3" id
    , simpleButton "-4 M3" id
    , simpleButton "-5 4" id
    , simpleButton "-6 tritone" id
    , simpleButton "-7 5" id
    , simpleButton "-8 m6" id
    , simpleButton "-9 M6" id
    , simpleButton "-10 m7" id
    , simpleButton "-11 M7" id
    , simpleButton "any:" id
    , return (never,  UI.input # set UI.id_ "halfsteps" )
    , return (never,  string "by scale degrees:" )
    , simpleButton "+1" id
    , simpleButton "+2" id
    , simpleButton "+3" id
    , simpleButton "+4" id
    , simpleButton "+5" id
    , simpleButton "+6" id
    , simpleButton "-1" id
    , simpleButton "-2" id
    , simpleButton "-3" id
    , simpleButton "-4" id
    , simpleButton "-5" id
    , simpleButton "-6" id  
    , simpleButton "any:" id
    , return (never,  UI.input # set UI.id_ "scaledegrees" )
    ]

inspect :: UI (Event [Mutator],UI Element)
inspect = makeDefaultPanel "Inspect"
    [ return (never,  string "data goes here" )
    ]

lilypond :: UI (Event [Mutator],UI Element)
lilypond = makeDefaultPanel "Lilypond"
    [ simpleButton "Add function" id
    , return (never,  row [string "\"\\fermata\"", UI.input # set UI.id_ "lyfunc"] )
    , simpleButton "Add command" id
    , return (never,  row [string "\"\\foo", UI.input # set UI.id_ "lycom", string "{ ... }"] )
    , simpleButton "Add post-fix lilypond item" id
    , return (never,  row [string "\"\\fermata\"", UI.input # set UI.id_ "lywith"] )
    , simpleButton "Remove all matching input:" id
    , simpleButton "Remove all but those matching input" id
    , return (never,  row [string "matching by", UI.input # set UI.id_ "lymatch"] )
    , simpleButton "portamento" id
    , simpleButton "trill" id
    , simpleButton "...." id
    ]

note :: UI (Event [Mutator],UI Element)
note = makeDefaultPanel "Note"
    [ simpleButton "A" appendNote
    , simpleButton "B" id
    , simpleButton "C" id
    , simpleButton "D" id
    , simpleButton "E" id
    , simpleButton "F" id
    , simpleButton "G" id
    , simpleButton "rest" id
    , simpleButton "special effect" id
    , return (never,  string "percussion:" )
    , simpleButton "hh" id
    , simpleButton "sn" id
    , simpleButton "..." id
    ]   

accidental :: UI (Event [Mutator],UI Element)
accidental = makeDefaultPanel "Accidental"
    [ simpleButton "remove" id
    , simpleButton "auto-assign" id
    , simpleButton "bb" id
    , simpleButton "b" id
    , simpleButton "-" id
    , simpleButton "#" id
    , simpleButton "##" id
    ]

durationpanel :: UI (Event [Mutator],UI Element)
durationpanel = makeDefaultPanel "Duration"
    [ return (never,  string "sets note just entered, or all in selection" )
    , simpleButton "\\breve" id 
    , simpleButton "1" id
    , simpleButton "2" id
    , simpleButton "4" id
    , simpleButton "8" id
    , simpleButton "16" id
    , simpleButton "32" id
    , simpleButton "64" id
    , simpleButton "128" id
    , simpleButton "add dot" id
    , simpleButton "remove dot" id
    , simpleButton "any rational duration:" id
    , return (never,  UI.input # set UI.id_ "setDur" )
    , return (never,  UI.br )
    , return (never,  string "multiply by:" )
    , simpleButton "1/2" id
    , simpleButton "2/3 (triplet)" id
    , simpleButton "4/5 (pentuplet)" id
    , simpleButton "4/7 (septuplet)" id
    , simpleButton "8/9" id
    , simpleButton "8/11" id
    , simpleButton "multiply by rational:" id
    , return (never,  UI.input # set UI.id_ "multiplyDur" )
    ]

automation :: UI (Event [Mutator],UI Element)
automation = makeDefaultPanel "Automation"
    [ return (never,  row [string "new notes given pitch according to their key", UI.input # set UI.type_ "checkbox" # set UI.id_ "newbykey"] )
    , return (never,  row [string "new notes inherit key", UI.input # set UI.type_ "checkbox" # set UI.id_ "inheritkey"] )
    , return (never,  row [string "new notes spelled according to key", UI.input # set UI.type_ "checkbox" # set UI.id_ "spelledbykey"] )
    , return (never,  row [string "new notes in nearest octave", UI.input # set UI.type_ "checkbox" # set UI.id_ "nearest octave"] )
    , return (never,  row [string "new notes inherit dynamic", UI.input # set UI.type_ "checkbox" # set UI.id_ "inherit dynamic"] )
    , return (never,  row [string "new notes inherit instrument", UI.input # set UI.type_ "checkbox" # set UI.id_ "inheritinstrument"] )
    , return (never,  row [string "new notes inherit clef", UI.input # set UI.type_ "checkbox" # set UI.id_ "inheritclef"] )
    , return (never,  row [string "recalculate clefs when adding a note", UI.input # set UI.type_ "checkbox" # set UI.id_ "recalcclef"] )
    , return (never,  row [string "recalculate keys when adding a note", UI.input # set UI.type_ "checkbox" # set UI.id_ "recalckey"] )
    ]

ties :: UI (Event [Mutator],UI Element)
ties = makeDefaultPanel "Ties"
    [ simpleButton "add tie" id
    , simpleButton "remove tie" id
    , simpleButton "merge tied notes" id
    ]

meter :: UI (Event [Mutator],UI Element)
meter = makeDefaultPanel "Meter"
    [ simpleButton "remove" id
    , simpleButton "2/2" id
    , simpleButton "2/4" id 
    , simpleButton "3/4" id 
    , simpleButton "4/4" id 
    , simpleButton "5/4" id 
    , simpleButton "6/4" id 
    , simpleButton "7/4" id 
    , simpleButton "3/8" id 
    , simpleButton "5/8" id 
    , simpleButton "6/8" id 
    , simpleButton "7/8" id 
    , simpleButton "9/8" id 
    , simpleButton "12/8" id 
    , simpleButton "x/y" id 
    , return (never,  row [string "x", UI.input # set UI.id_ "xMeter"] )
    , return (never,  row [string "y", UI.input # set UI.id_ "yMeter"] )
    , simpleButton "sum of fractions meter" id
    , return (never,  column [string "format: (a+b+c)/d + (e+f)/g", UI.input # set UI.id_ "abcMeter"] )
    ]

marks :: UI (Event [Mutator],UI Element)
marks = makeDefaultPanel "Marks"
    [ simpleButton "add rehearsal mark" id 
    , simpleButton "add markup rehearsal mark" id
    , return (never,  UI.input # set UI.id_ "reheasalMarkup" )
    ]

functions :: UI (Event [Mutator],UI Element)
functions = makeDefaultPanel "Functions"
    [ simpleButton "product" id 
    ]

graph :: UI (Event [Mutator],UI Element)
graph = makeDefaultPanel "Graph"
    [ totalButton "undo" id
    , return (never,  string "parents: ..." )
    , totalButton "redo" id
    , return (never,  string "children: ..." )
    , totalButton "identity action" id
    , totalButton "copy history from here forward" id
    , return (never,  UI.input )
    , return (never,  string "attributes for this node..." )
    ]

trim :: UI (Event [Mutator],UI Element)
trim = makeTrimPanel "Trim"
    [ totalButton "remove history not leading to this node" id
    , totalButton "remove history not leading to any of:" id
    , return (never,  UI.input )
    , totalButton "remove saved values for nodes, excluding:" id
    , return (never,  UI.input )
    ]