{-# LANGUAGE QuasiQuotes #-}
module HeqetApp.Functions where

import HeqetApp.Types
import Heqet.Types
import Heqet.Tools
import Heqet.Input.English
import Control.Lens

appendNote :: Music -> Music
appendNote [] = [music| c1 |]
appendNote mus = let
    end = getEndTime mus
    note = [music| c1 |] & traverse.t .~ end
    in mus `seqI` note
 