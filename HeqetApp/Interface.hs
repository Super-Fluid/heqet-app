module HeqetApp.Interface 
    (panels
    ,navbar
    ) where

import HeqetApp.Types
import HeqetApp.Buttons
import qualified HeqetApp.Interface.Navbar as Nav

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Events
import Data.IORef

navbar :: IORef ViewState -> UI Element
navbar state = row
    [ totalButton "|<"  (Nav.toStart    ) state
    , totalButton "<"   (Nav.backward   ) state
    , totalButton ">"   (Nav.forward    ) state
    , totalButton ">|"  (Nav.toEnd      ) state
    , totalButton "^"   (Nav.up         ) state
    , totalButton "v"   (Nav.down       ) state
    , totalButton "<->" (Nav.stretch    ) state
    , totalButton ">-<" (Nav.squeeze    ) state
    , totalButton "+"   (Nav.larger     ) state
    , totalButton "-"   (Nav.smaller    ) state
    ]

panels :: [(IORef AppState -> UI Element)]
panels =
    [ file
    , edit
    , select
    , label
    , view
    
    , note
    , accidental
    , duration
    , dynamics
    , articulation 
    , ties  
    , instrument
    , key
    , clef
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

--navigation state = UI.div # set text "<<< << < > >> >>> up down >-< <-> (+) (-)"

file :: (IORef AppState) -> UI Element
file state = makeDefaultPanel state "File" 
    [ const $ heqetpath
    , ioButton "Open" return
    , ioButton "Save" return
    , ioButton "Embed" return
    , const $ lypath
    , ioButton "Export Lilypond code" return
    ]
    where 
        heqetpath = UI.input
        lypath = UI.input

edit :: (IORef AppState) -> UI Element
edit state = makeDefaultPanel state "Edit" 
    [ totalButton "copy" id
    , totalButton "paste" id
    , totalButton "paste without history" id
    , totalButton "paste overlapping" id
    , const $ string "paste only:"
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
    , const $ row [totalButton "create ostinato until" id state, UI.input]
    ]

articulation :: (IORef AppState) -> UI Element
articulation state = makeDefaultPanel state "Articulation" 
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

fingering :: (IORef AppState) -> UI Element
fingering state = makeDefaultPanel state "Fingering" 
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

dynamics :: (IORef AppState) -> UI Element
dynamics state = makeDefaultPanel state "Dynamics" 
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
    , const $ string "other dynamic marks:"
    , simpleButton "fp" id
    , simpleButton "sfz" id
    ]

instrument :: (IORef AppState) -> UI Element
instrument state = makeDefaultPanel state "Instrument" 
    [ simpleButton "assign standard instrument" id
    , const $ UI.input
    , simpleButton "remove" id
    , const $ string "info on instrument"
    ]

key :: (IORef AppState) -> UI Element
key state = makeDefaultPanel state "Key" 
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

clef :: (IORef AppState) -> UI Element
clef state = makeDefaultPanel state "Clef" 
    [ simpleButton "auto" id
    , simpleButton "remove" id
    , simpleButton "treble" id
    , simpleButton "alto" id
    , simpleButton "treble_8" id
    , simpleButton "tenor" id
    , simpleButton "bass" id
    , const $ string "manual clefs are only apparent in transposed view!"
    ]

view :: (IORef AppState) -> UI Element
view state = makeDefaultPanel state "View" 
    [ const $ UI.button # set text "concert"
    , const $ UI.button # set text "transposed"
    ]

select :: (IORef AppState) -> UI Element
select state = makeDefaultPanel state "Select" 
    [ totalButton "all" id
    , totalButton "none" id
    , totalButton "invert" id
    , const $ row [ string "insertion point:", UI.input ]
    , const $ string "refine selection by:"
    , filterRow "start time" (const $ const $ id)
    , filterRow "end time" (const $ const $ id)
    , filterRow "duration" (const $ const $ id)
    , filterRow "pitch" (const $ const $ id)
    , filterRow "dynamic" (const $ const $ id)
    , filterRow "time from measure start" (const $ const $ id)
    , const $ row [ uiTotalButton "staff ==" return state, UI.input ]
    , const $ row [ uiTotalButton "instrument ==" return state, UI.input ]
    , const $ row [ uiTotalButton "has lilypond:" return state, UI.input ]
    , const $ row [ uiTotalButton "measures at time:" return state, UI.input ]
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
    , const $ string "expand selection by:"
    , totalButton "hold time, relax staff" id
    , totalButton "hold time, relax substaff" id
    , totalButton "hold time and staff, relax voice" id
    , totalButton "hold time and substaff, relax voice" id
    , totalButton "same rhythm" id
    , totalButton "same rhythm on staff" id
    , totalButton "same rhythm on substaff" id
    , totalButton "same instrument" id
    ]

label :: (IORef AppState) -> UI Element
label state = makeDefaultPanel state "Label" 
    [ const $ column (map (($state).colorRow) colorClasses)
    , simpleButton "remove all colors" id
    , const $ string "string labels:"
    , uiTotalButton "select" return
    , uiTotalButton "deselect" return
    , uiSimpleButton "add" return
    , uiSimpleButton "remove" return
    , const $ UI.input # set UI.id_ "string-label"
    , uiSimpleButton "remove all string labels" return
    ]

colorClasses = 
    ["darkgreen"
    ,"brown"
    ,"darkblue"
    ,"red"
    ,"lightblue"
    ,"purple"
    ,"yellow"
    ,"orange"
    ,"pink"
    ,"grey"
    ]

colorRow :: String -> (IORef AppState) -> UI Element
colorRow bid state = row [sel, desel, clr, unclr] where
    sel = totalButton "select" id state #. ("color-"++bid)
    desel = totalButton "deselect" id state #. ("color-"++bid)
    clr = simpleButton "color" id state #. ("color-"++bid)
    unclr = simpleButton "uncolor" id state #. ("color-"++bid)

transpose :: (IORef AppState) -> UI Element
transpose state = makeDefaultPanel state "Transpose" 
    [ simpleButton "oct up" id
    , simpleButton "oct down" id
    , const $ string "by halfsteps:"
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
    , uiSimpleButton "any:" return
    , const $ UI.input # set UI.id_ "halfsteps"
    , const $ string "by scale degrees:"
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
    , uiSimpleButton "any:" return
    , const $ UI.input # set UI.id_ "scaledegrees"
    ]

inspect :: (IORef AppState) -> UI Element
inspect state = makeDefaultPanel state "Inspect" 
    [ const $ string "data goes here"
    ]

lilypond :: (IORef AppState) -> UI Element
lilypond state = makeDefaultPanel state "Lilypond" 
    [ uiSimpleButton "Add function" return
    , const $ row [string "\"\\fermata\"", UI.input # set UI.id_ "lyfunc"]
    , uiSimpleButton "Add command" return
    , const $ row [string "\"\\foo", UI.input # set UI.id_ "lycom", string "{ ... }"]
    , uiSimpleButton "Add post-fix lilypond item" return
    , const $ row [string "\"\\fermata\"", UI.input # set UI.id_ "lywith"]
    , uiSimpleButton "Remove all matching input:" return
    , uiSimpleButton "Remove all but those matching input" return
    , const $ row [string "matching by", UI.input # set UI.id_ "lymatch"]
    , simpleButton "portamento" id
    , simpleButton "trill" id
    , simpleButton "...." id
    ]

note :: (IORef AppState) -> UI Element
note state = makeDefaultPanel state "Note" 
    [ simpleButton "A" id
    , simpleButton "B" id
    , simpleButton "C" id
    , simpleButton "D" id
    , simpleButton "E" id
    , simpleButton "F" id
    , simpleButton "G" id
    , simpleButton "rest" id
    , simpleButton "special effect" id
    , const $ string "percussion:"
    , simpleButton "hh" id
    , simpleButton "sn" id
    , simpleButton "..." id
    ]

accidental :: (IORef AppState) -> UI Element
accidental state = makeDefaultPanel state "Accidental" 
    [simpleButton "remove" id
    , simpleButton "auto-assign" id
    , simpleButton "bb" id
    , simpleButton "b" id
    , simpleButton "-" id
    , simpleButton "#" id
    , simpleButton "##" id
    ]

duration :: (IORef AppState) -> UI Element
duration state = makeDefaultPanel state "Duration" 
    [ const $ string "sets note just entered, or all in selection"
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
    , uiSimpleButton "any rational duration:" return
    , const $ UI.input # set UI.id_ "setDur"
    , const $ UI.br
    , const $ string "multiply by:"
    , simpleButton "1/2" id
    , simpleButton "2/3 (triplet)" id
    , simpleButton "4/5 (pentuplet)" id
    , simpleButton "4/7 (septuplet)" id
    , simpleButton "8/9" id
    , simpleButton "8/11" id
    , uiSimpleButton "multiply by rational:" return
    , const $ UI.input # set UI.id_ "multiplyDur"
    ]

automation :: (IORef AppState) -> UI Element
automation state = makeDefaultPanel state "Automation" 
    [ const $ row [string "new notes given pitch according to their key", UI.input # set UI.type_ "checkbox" # set UI.id_ "newbykey"]
    , const $ row [string "new notes inherit key", UI.input # set UI.type_ "checkbox" # set UI.id_ "inheritkey"]
    , const $ row [string "new notes spelled according to key", UI.input # set UI.type_ "checkbox" # set UI.id_ "spelledbykey"]
    , const $ row [string "new notes in nearest octave", UI.input # set UI.type_ "checkbox" # set UI.id_ "nearest octave"]
    , const $ row [string "new notes inherit dynamic", UI.input # set UI.type_ "checkbox" # set UI.id_ "inherit dynamic"]
    , const $ row [string "new notes inherit instrument", UI.input # set UI.type_ "checkbox" # set UI.id_ "inheritinstrument"]
    , const $ row [string "new notes inherit clef", UI.input # set UI.type_ "checkbox" # set UI.id_ "inheritclef"]
    , const $ row [string "recalculate clefs when adding a note", UI.input # set UI.type_ "checkbox" # set UI.id_ "recalcclef"]
    , const $ row [string "recalculate keys when adding a note", UI.input # set UI.type_ "checkbox" # set UI.id_ "recalckey"]
    ]

ties :: (IORef AppState) -> UI Element
ties state = makeDefaultPanel state "Ties" 
    [ simpleButton "add tie" id
    , simpleButton "remove tie" id
    , simpleButton "merge tied notes" id
    ]

meter :: (IORef AppState) -> UI Element
meter state = makeDefaultPanel state "Meter" 
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
    , uiSimpleButton "x/y" return 
    , const $ row [string "x", UI.input # set UI.id_ "xMeter"]
    , const $ row [string "y", UI.input # set UI.id_ "yMeter"]
    , uiSimpleButton "sum of fractions meter" return
    , const $ column [string "format: (a+b+c)/d + (e+f)/g", UI.input # set UI.id_ "abcMeter"]
    ]

marks :: (IORef AppState) -> UI Element
marks state = makeDefaultPanel state "Marks" 
    [ simpleButton "add rehearsal mark" id 
    , uiSimpleButton "add markup rehearsal mark" return
    , const $ UI.input # set UI.id_ "reheasalMarkup"
    ]

functions :: (IORef AppState) -> UI Element
functions state = makeDefaultPanel state "Functions" 
    [ simpleButton "product" id 
    ]

graph :: (IORef AppState) -> UI Element
graph state = makeDefaultPanel state "Graph" 
    [ totalButton "undo" id
    , const $ string "parents: ..."
    , totalButton "redo" id
    , const $ string "children: ..."
    , totalButton "identity action" id
    , totalButton "copy history from here forward" id
    , const $ UI.input
    , const $ string "attributes for this node..."
    ]

trim :: (IORef AppState) -> UI Element
trim state = set UI.id_ "trim" $ makeDefaultPanel state "Trim" 
    [ totalButton "remove history not leading to this node" id
    , totalButton "remove history not leading to any of:" id
    , const $ UI.input
    , totalButton "remove saved values for nodes, excluding:" id
    , const $ UI.input
    ]