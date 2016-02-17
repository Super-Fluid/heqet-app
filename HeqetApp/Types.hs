{-# LANGUAGE TemplateHaskell #-}

module HeqetApp.Types where

import Control.Lens

type AppState = (Int,Music)

type Music = () -- will change to Heqet soon

data Comparison = LE | LEQ | Equal | GEQ | GE
    deriving (Show,Eq)

data AddRemove = Add | Remove
    deriving (Show,Eq)

{- will be taken from the heqet library -}

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp 
    deriving (Eq, Ord, Enum, Bounded,Show,Read)

data Clef = Treble | Alto | Treble8 | Tenor | Bass | CustomClef String
    deriving (Eq, Show, Read)

data SimpleArticulation = Marcato | Stopped | Tenuto | Staccatissimo | Accent | Staccato | Portato
    deriving (Eq, Show, Read)

type PointInTime = Rational

-- New types:

-- Probably move these into the Heqet library..

type StaffPosition = Int -- probably between -16 and 16 or not much beyond that

data UpDown = Up | Down
data Rest = RBreve | R1 | R2 | R4 | R8 | R16 | R32 | R64 | R128 

type TextDynamic = String
type TextMeter = String

data NoteHead = X | Breve | Whole | Half | Filled
type NumFlags = Int

type StaffN = Int

data Symbol' = 
    NoteHead NoteHead
    | Stem UpDown
    | Flags UpDown NumFlags
    | Accidental Accidental
    | SimpleArticulation UpDown SimpleArticulation
    | Markup String
    | TextDynamic TextDynamic
    | TextMeter TextMeter
    | KeyChange Int
    | Barline
    | Clef Clef
    | Rest
    | Tie
    | Slur UpDown
    | LedgerLines

data HeadingSymbol' = 
    ClefH Clef
    | KeyH Int
    | TextMeterH TextMeter
    | TopStaffBracket
    | BottomStaffBracket
    | InstName String

type Symbol = (Symbol',StaffN,StaffPosition,PointInTime)
type HeadingSymbol = (HeadingSymbol',StaffN)

data ViewState = ViewState
    { _startTime :: PointInTime
    , _endTime :: PointInTime
    , _topStaff :: StaffN
    , _staffSize :: Int -- number of pixels between two staff lines
    }

exampleSymbols :: [Symbol]
exampleSymbols =
    [ (NoteHead Whole,1,0,0)
    , (NoteHead Whole,1,3,1)
    , (NoteHead Whole,1,2,2)
    , (NoteHead Filled,1,(-1),3)
    , (NoteHead Filled,1,-3,13/4)
    , (NoteHead Filled,1,-2,14/4)
    , (NoteHead Filled,1,-5,15/4)
    , (NoteHead Filled,1,4,4)
    , (Flags Down 3,1,4,4)
    , (Stem Down,1,4,4)
    , (Accidental Sharp,1,4,4)
    , (SimpleArticulation Down Accent,1,4,4)
    ]

exampleHeading :: [HeadingSymbol]
exampleHeading =
    [ (ClefH Treble,1)
    , (KeyH (-4), 1)
    , (TextMeterH "4/4",1)
    ]

makeLenses ''ViewState