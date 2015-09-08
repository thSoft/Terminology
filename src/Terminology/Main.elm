module Terminology.Main where

import Html exposing (Html)
import Terminology.Model exposing (..)
import Terminology.Update exposing (..)
import Terminology.View exposing (..)

main : Signal Html
main =
  Signal.map (view updates.address) modelState

modelState : Signal Model
modelState =
  Signal.foldp (<|) initialModel updates.signal

updates : Signal.Mailbox Update
updates =
  Signal.mailbox noOp
