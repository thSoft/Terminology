module Combobox where

import Array
import String
import Regex
import Time
import Signal exposing (Address, Message)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Task exposing (Task)
import Task.Extra
import Effects exposing (Effects, Never)
import Keyboard.Keys exposing (..)

-- TODOs
-- feature: highlight occurrences
-- feature: description for items

-- Model

type alias Props =
  {
    items: List Item,
    style: Style,
    htmlAttributes: List Html.Attribute
  }

type alias Item =
  {
    label: String,
    task: Task Never ()
  }

type Style =
  Input |
  ContentEditable

type alias State =
  {
    inputText: String,
    contentShouldBeSet: Bool,
    selectedIndex: Int
  }

initialState : State
initialState =
  {
    inputText =
      "",
    contentShouldBeSet =
      False,
    selectedIndex =
      0
  }

-- Update

type Action =
  Action {
    updateState: UpdateState,
    effects: Effects Action
  }

type alias UpdateState =
  Props -> State -> State

action : UpdateState -> Action
action updateState =
  Action {
    updateState =
      updateState,
    effects =
      Effects.none
  }

choose : Item -> Action
choose item =
  Action {
    updateState =
      setInputText True "",
    effects =
      item.task
      |> Task.map (\_ -> setInputText False "" |> action)
      |> Task.Extra.delay (50 * Time.millisecond) -- XXX to avoid race condition of tasks and rendering?
      |> Effects.task
  }

update : Action -> Props -> State -> (State, Effects Action)
update (Action action) props state =
  let updatedState =
        state |> action.updateState props
      effects =
        action.effects
  in (updatedState, effects)

noOp : UpdateState
noOp props state =
  state

setInputText : Bool -> String -> UpdateState
setInputText contentShouldBeSet inputText props state =
  { state |
    inputText <-
      inputText,
    contentShouldBeSet <-
      contentShouldBeSet,
    selectedIndex <-
      0
  }

moveToNext : UpdateState
moveToNext props state =
  moveBy 1 props state

moveToPrevious : UpdateState
moveToPrevious props state =
  moveBy -1 props state

moveBy : Int -> UpdateState
moveBy delta props state =
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

moveToFirst : UpdateState
moveToFirst props state =
  { state |
    selectedIndex <-
      0
  }

moveToLast : UpdateState
moveToLast props state =
  { state |
    selectedIndex <-
      (getVisibleItems props state |> List.length) - 1
  }

-- View

view : Address Action -> Props -> State -> Html
view address props state =
  let result =
        Html.span
          []
          (input :: menu)
      input =
        case props.style of
          Input ->
            Html.input
              attributes
              []
          ContentEditable ->
            Html.span
              (Attributes.contenteditable True :: attributes)
              []
      attributes =
        (eventHandlers ++ textContent ++ props.htmlAttributes)
      eventHandlers =
        [
          Attributes.attribute "onkeydown"
            "if ([13, 38, 40].indexOf(event.keyCode) > -1) {
              event.preventDefault();
            }", -- XXX https://github.com/evancz/elm-html/issues/83
          Events.on "input" (inputTextDecoder props.style) handleInput,
          Events.on "keyup" Events.keyCode handleKeyUp
        ]
      submitAction =
        (visibleItems |> Array.fromList |> Array.get state.selectedIndex)
        |> Maybe.map (\selectedItem -> choose selectedItem)
        |> Maybe.withDefault (noOp |> action)
      handleKeyUp key =
        let updateState =
              if | key == arrowDown.keyCode -> moveToNext |> action
                 | key == arrowUp.keyCode -> moveToPrevious |> action
                 | key == pageDown.keyCode -> moveToLast |> action
                 | key == pageUp.keyCode -> moveToFirst |> action
                 | key == enter.keyCode -> submitAction
                 | otherwise -> noOp |> action
        in updateState |> Signal.message address
      handleInput inputText =
        setInputText False inputText |> action |> Signal.message address
      textContent =
        if state.contentShouldBeSet then
          case props.style of
            Input ->
              [Attributes.value state.inputText]
            ContentEditable ->
              [Attributes.property "textContent" (state.inputText |> Encode.string)]
        else
          []
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
                  ("padding", "2px") ::
                  (selectedStyle (index == state.selectedIndex))
                ),
                Events.onClick address (choose item)
              ]
              [
                Html.text item.label
              ]
          )
      visibleItems =
        getVisibleItems props state
      selectedStyle selected =
        if selected then
          [
            ("background", "linear-gradient(to bottom, #0066ee, #0033cc)"),
            ("color", "white")
          ]
        else
          []
 in result

inputTextDecoder : Style -> Decoder String
inputTextDecoder style =
  case style of
    Input ->
      Events.targetValue
    ContentEditable ->
      Decode.at ["target", "textContent"] Decode.string

getVisibleItems : Props -> State -> List Item
getVisibleItems props state =
  props.items |> List.filter (\item ->
    item.label |> fuzzyContains state.inputText
  )

fuzzyContains : String -> String -> Bool
fuzzyContains needle haystack =
  needle |> String.words |> List.all (\word ->
    if word |> String.isEmpty then
      False
    else
      haystack |> containsIgnoreCase word
  )

containsIgnoreCase : String -> String -> Bool
containsIgnoreCase needle haystack =
  haystack |> Regex.contains (needle |> Regex.escape |> Regex.regex |> Regex.caseInsensitive)
