{-# LANGUAGE TemplateHaskell #-}

module HeqetApp.Types where

import Heqet.Types

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Canvas 
    (FillStyle
    ,htmlColor
    )

import Control.Lens

type AppState = (Int,Music)
startingAppState = (0,[]) :: AppState

data Comparison = LE | LEQ | Equal | GEQ | GE
    deriving (Show,Eq)

data AddRemove = Add | Remove
    deriving (Show,Eq)

data Mutator = PureMutator (AppState -> AppState) | IOMutator (AppState -> IO AppState)

exampleSymbols :: [Symbol]
exampleSymbols =
    [ (NoteHead Whole,1,0,0)
    , (NoteHead Whole,1,3,1)
    , (Barline,1,0,5)
    , (NoteHead Whole,1,2,2)
    , (NoteHead Filled,1,(-1),3)
    , (Stem Down,1,(-1),3)
    , (NoteHead Filled,1,-3,13/4)
    , (Stem Down,1,-3,13/4)
    , (Accidental DoubleSharp,1,-3,13/4)
    , (NoteHead Filled,1,-2,14/4)
    , (Stem Down,1,-2,14/4)
    , (Flags Down 1,1,-2,14/4)
    , (NoteHead Filled,1,-5,15/4)
    , (Stem Down,1,-5,15/4)
    , (Flags Down 1, 1,-5,15/4)
    , (NoteHead X,1,4,4)
    , (Flags Down 3,1,4,4)
    , (Stem Down,1,4,4)
    , (Accidental Sharp,1,4,4)
    , (SimpleArticulation Down Accent,1,4,4)
    , (NoteHead Half,2,2,1)
    , (Accidental Flat,2,2,1)
    , (Stem Up,2,2,1)
    , (SimpleArticulation Up Tenuto,2,2,1)
    , (NoteHead Breve,2,3,3/2)
    , (SimpleArticulation Up Staccato,2,3,3/2)
    , (Accidental DoubleFlat,2,3,3/2)
    , (Rest RBreve,3,0,1)
    , (Rest R1,3,0,3/2)
    , (Rest R2,3,0,2)
    , (Rest R4,3,0,3)
    , (Rest R8,3,0,4)
    , (Rest R16,3,0,17/4)
    , (Rest R32,3,0,18/4)
    , (Rest R64,3,0,19/4)
    , (Rest R128,3,0,5)
    , (Color Red,4,0,1)
    , (Color Red,4,2,2)
    , (Color Red,4,4,3)
    , (Color Red,4,9,5)
    , (Color DarkGreen,4,4,3)
    , (Color Brown,4,4,3)
    , (Color LightBlue,4,4,3)
    , (Color DarkBlue,4,4,3)
    , (Color Pink,4,4,3)
    , (Color Orange,4,4,3)
    , (Color Yellow,4,4,3)
    , (Color Grey,4,4,3)
    , (Color Purple,4,4,3)
    , (NoteHead Whole,4,0,1)
    , (NoteHead Whole,4,2,2)
    , (NoteHead Half,4,4,3)
    , (Stem Down,4,4,3)
    , (NoteHead Whole,4,9,5)
    , (LedgerLines,4,9,5)
    , (Color Red,4,-9,6)
    , (NoteHead Half,4,-9,6)
    , (Stem Up,4,-9,6)
    , (LedgerLines,4,-9,6)
    , (Color Red,4,-10,13/2)
    , (NoteHead Half,4,-10,13/2)
    , (Stem Up,4,-10,13/2)
    , (LedgerLines,4,-10,13/2)
    , (Accidental Natural,4,-10,13/2)
    , (NoteHead Filled,2,0,8)
    , (NoteHead Filled,2,0,9)
    , (NoteHead Filled,2,0,10)
    , (NoteHead Filled,2,0,11)
    , (NoteHead Filled,2,0,12)
    , (NoteHead Filled,2,0,13)
    , (NoteHead Filled,2,0,14)
    , (Stem Down,2,0,8)
    , (SimpleArticulation Up Portato,2,0,8)
    , (Stem Down,2,0,9)
    , (SimpleArticulation Down Portato,2,0,9)
    , (Stem Down,2,0,10)
    , (Stem Down,2,0,11)
    , (Stem Down,2,0,12)
    , (Stem Down,2,0,13)
    , (Stem Down,2,0,14)
    , (Selection,2,0,11)
    , (Selection,2,0,9)
    , (Selection,2,0,12)
    ]

exampleHeading :: [HeadingSymbol]
exampleHeading =
    [ (ClefH Treble,1)
    , (ClefH Bass,4)
    , (ClefH Alto,3)
    , (ClefH Treble8,2)
    , (KeyH (-4), 1)
    , (KeyH (3), 2)
    , (TextMeterH "4/4",1)
    ]

type PX = Double

data ViewState = ViewState
    { _startTime :: PointInTime
    , _endTime :: PointInTime
    , _timeScale :: PX -- number of pixels in 1 whole note of time
    , _topStaff :: StaffN
    , _bottomStaff :: StaffN
    , _staffSize :: PX -- number of pixels between two staff lines
    }
    deriving (Eq,Show,Read)

makeLenses ''ViewState