module HeqetApp.Interface.Navbar where

import HeqetApp.Types

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Attributes

import Control.Lens hiding ((#))

{-
    Note: these functions do NOT look at the canvas
    size -- they can't, since they're pure.
    The canvas size must be read somewhere in HeqetApp.hs ...
-}

toStart  :: ViewState -> ViewState
toStart     vs = let
    timeSpan = vs^.endTime - vs^.startTime
    in vs & endTime .~ timeSpan & startTime .~ 0

backward :: ViewState -> ViewState
backward    vs = let
    timeSpan = vs^.endTime - vs^.startTime
    nudge = timeSpan / 4
    in vs & startTime %~ (subtract nudge) & endTime  %~ (subtract nudge)

forward  :: ViewState -> ViewState
forward     vs = let
    timeSpan = vs^.endTime - vs^.startTime
    nudge = timeSpan / 4
    in vs & startTime %~ (+ nudge) & endTime  %~ (+ nudge)

toEnd    :: ViewState -> ViewState
toEnd       vs = vs -- TODO

up       :: ViewState -> ViewState
up          vs = vs & topStaff %~ (subtract 1) & bottomStaff %~ (subtract 1)

down     :: ViewState -> ViewState
down        vs = vs & topStaff %~ (+ 1) & bottomStaff %~ (+ 1)

stretch  :: ViewState -> ViewState
stretch     vs = let
    timeSpan = vs^.endTime - vs^.startTime
    in vs & endTime %~ (subtract (timeSpan/2)) & timeScale %~ (*2)


squeeze  :: ViewState -> ViewState
squeeze     vs = let
    timeSpan = vs^.endTime - vs^.startTime
    in vs & endTime %~ (+timeSpan) & timeScale %~ (/2)

larger   :: ViewState -> ViewState
larger      vs = vs & staffSize %~ (*1.2)

smaller  :: ViewState -> ViewState
smaller     vs = vs & staffSize %~ (/1.2)
