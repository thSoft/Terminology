module Terminology.Main where

import Task exposing (Task)
import Effects exposing (Never)
import Html exposing (Html)
import StartApp exposing (App)
import Terminology.Model exposing (..)
import Terminology.Update exposing (..)
import Terminology.Init exposing (..)
import Terminology.View exposing (..)

app : App Model
app =
  StartApp.start {
    init = init,
    update = update,
    view = view,
    inputs = []
  }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks
