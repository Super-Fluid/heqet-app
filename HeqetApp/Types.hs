module HeqetApp.Types where

type AppState = (Int,Music)

type Music = () -- will change to Heqet soon

data Comparison = LE | LEQ | Equal | GEQ | GE
    deriving (Show,Eq)

data AddRemove = Add | Remove
    deriving (Show,Eq)