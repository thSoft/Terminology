module Terminology.Combobox where

import Array
import String
import Signal exposing (Address, Message)
import Json.Decode as Decode
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Keyboard.Keys as Keys

-- Model

type alias Props =
  {
    items: List Item,
    inputAttributes: List Html.Attribute
  }

type alias Item =
  {
    label: String,
    message: Maybe Message
  }

type alias State =
  {
    inputText: String,
    selectedIndex: Int
  }

initialState : State
initialState =
  {
    inputText =
      "",
    selectedIndex =
      0
  }

-- Update

type alias Update =
  Props -> State -> State

noOp : Update
noOp props state =
  state

setInputText : String -> Update
setInputText inputText' props state =
  { state |
    inputText <-
      inputText'
  }

next : Update
next props state =
  moveSelectionBy 1 props state

previous : Update
previous props state =
  moveSelectionBy -1 props state

moveSelectionBy : Int -> Update
moveSelectionBy delta props state =
  let result =
        if visibleItems |> List.isEmpty then
          state
        else
          { state |
            selectedIndex <-
              (state.selectedIndex + delta) % (visibleItems |> List.length)
          }
      visibleItems =
        getVisibleItems props state
  in result

first : Update
first props state =
  { state |
    selectedIndex <-
      0
  }

last : Update
last props state =
  { state |
    selectedIndex <-
      (getVisibleItems props state |> List.length) - 1
  }

-- View

view : Address Update -> Props -> State -> Html
view address props state =
  let result =
        Html.div
          []
          (input :: menu)
      input =
        Html.input
          ([
            Attributes.value state.inputText,
            Events.on "input" Events.targetValue (\inputText' ->
              Signal.message address (setInputText inputText')
            ),
            Events.on "keydown" Events.keyCode handleKeyPress
          ] ++ props.inputAttributes)
          []
      handleKeyPress key =
        if | key == (Keys.arrowDown |> .keyCode) -> next |> Signal.message address
           | key == (Keys.arrowUp |> .keyCode) -> previous |> Signal.message address
           | key == (Keys.enter |> .keyCode) ->
             (visibleItems
             |> Array.fromList
             |> Array.get state.selectedIndex)
             `Maybe.andThen` .message
             |> Maybe.withDefault noOpMessage
           | otherwise -> noOpMessage
      noOpMessage =
        noOp |> Signal.message address
      menu =
        if items |> List.isEmpty then
          []
        else
          [
            Html.div
              [
                Attributes.style [
                  ("position", "relative"),
                  ("display", "table"),
                  ("box-shadow", "0px 2px 2px 2px #aaaaaa")
                ]
              ]
              items
          ]
      items =
        if state.inputText == "" then
          []
        else
          visibleItems
          |> List.indexedMap (\index item ->
            Html.div
              [
                Attributes.style (
                  selectedStyle (index == state.selectedIndex)
                  ++ [("padding", "2px")]),
                Events.on "click" (Decode.succeed ()) (always (handleClick item))
              ]
              [
                Html.text item.label
              ]
          )
      handleClick item =
        item.message |> Maybe.withDefault noOpMessage
      visibleItems =
        getVisibleItems props state
      selectedStyle selected =
        if selected then
          [
            ("background", "linear-gradient(to bottom, #0066ee, #0033cc)"),
            ("color", "white")]
        else
          []
 in result

getVisibleItems : Props -> State -> List Item
getVisibleItems props state =
  props.items |> List.filter (\item ->
    item.label |> String.contains state.inputText
  )
