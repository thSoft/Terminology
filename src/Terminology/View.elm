module Terminology.View where

import String
import Dict exposing (Dict)
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html.Events as Events
import Html.Attributes as Attributes
import Table exposing (..)
import Terminology.Model exposing (..)
import Terminology.Update exposing (..)
import Combobox

view : Address Action -> Model -> Html
view address model =
  Html.div
    [
      Attributes.style [
        ("font-family", "Baskerville, Georgia")
      ]
    ]
    [
      Html.h1 [] [Html.text "Terminology Editor"],
      model.rootTermViews |> viewTermViewRefs address model Nothing,
      viewCommandInput address model
    ]

viewTermViewRefs : Address Action -> Model -> Maybe (Reference TermView) -> List (Reference TermView) -> Html
viewTermViewRefs address model maybeParentTermViewRef termViewRefs =
  Html.dl
    []
    (termViewRefs |> List.map (viewTermViewRef address model maybeParentTermViewRef))

viewTermViewRef : Address Action -> Model -> Maybe (Reference TermView) -> Reference TermView -> Html
viewTermViewRef address model maybeParentTermViewRef termViewRef =
  termViewRef.get model.termViews
  |> Maybe.map (viewTermView address model maybeParentTermViewRef termViewRef)
  |> Maybe.withDefault (Html.div [] [Html.text "Referenced term view not found!"])

viewTermView : Address Action -> Model -> Maybe (Reference TermView) -> Reference TermView -> TermView -> Html
viewTermView address model maybeParentTermViewRef termViewRef (TermView termViewInfo) =
  termViewInfo.term.get model.terms
  |> Maybe.map (viewTerm address model maybeParentTermViewRef termViewRef (TermView termViewInfo))
  |> Maybe.withDefault (Html.div [] [Html.text "Referenced term not found!"])

viewTerm : Address Action -> Model -> Maybe (Reference TermView) -> Reference TermView -> TermView -> Term -> Html
viewTerm address model maybeParentTermViewRef termViewRef (TermView termViewInfo) (Term termInfo) =
  let result =
        Html.div
          [
            termViewStyle "table"
          ]
          [
            close,
            delete,
            name,
            definition,
            relatedTerms
          ]
      name =
        termInfo.name |> viewName
      definition =
        termInfo.definition |> viewDefinition address model termViewRef
      relatedTerms =
        termViewInfo |> .related |> viewRelated address model termViewRef
      delete =
        Html.img
          [
            Attributes.src "http://iconshow.me/media/images/Mixed/line-icon/png/16/trash-16.png",
            Attributes.title "Delete",
            Events.onClick address (deleteTerm maybeParentTermViewRef termViewRef |> action),
            Attributes.style [
              ("float", "right")
            ]
          ]
          []
      close =
        Html.img
          [
            Attributes.src "https://upload.wikimedia.org/wikipedia/commons/f/f8/Tooltip-CloseButton.png",
            Attributes.title "Close",
            Events.onClick address (closeTermView maybeParentTermViewRef termViewRef |> action),
            Attributes.style [
              ("float", "right")
            ]
          ]
          []
  in result

termViewStyle : String -> Attribute
termViewStyle display =
  Attributes.style [
    ("display", display),
    ("padding", "5px"),
    ("margin", "5px"),
    ("border-radius", "2px"),
    ("box-shadow", "0px 0px 2px 2px #aaaaaa")
  ]

viewName : String -> Html
viewName name =
  Html.dt
    [
      Attributes.style [
        ("font-weight", "bold")
      ]
    ]
    [Html.text name]

viewDefinition : Address Action -> Model -> Reference TermView -> Definition -> Html
viewDefinition address model termViewRef definition =
  Html.dt
    []
    (definition |> List.map (viewSegment address model termViewRef))

viewSegment : Address Action -> Model -> Reference TermView -> Segment -> Html
viewSegment address model termViewRef segment =
  case segment of
    Text text ->
      Html.text text
    TermReference relatedTermRef ->
      let result =
            Html.a
              [Events.onClick address (openTermView (Just termViewRef) relatedTermRef |> action)]
              [
                Html.abbr
                  [
                    Attributes.title relatedTermDefinition,
                    Attributes.style [
                      ("cursor", "pointer"),
                      ("text-decoration", "underline"),
                      ("text-decoration-style", "dotted")
                    ]
                  ]
                  [Html.text relatedTermName]
              ]
          relatedTermDefinition = relatedTermRef |> showRelatedTermDefinition model
          relatedTermName = relatedTermRef |> showRelatedTermName model
      in result

showRelatedTermDefinition : Model -> Reference Term -> String
showRelatedTermDefinition model relatedTermRef =
  relatedTermRef.get model.terms
  |> Maybe.map termInfo
  |> Maybe.map .definition
  |> Maybe.map (showDefinition model)
  |> Maybe.withDefault unknownTermLabel

unknownTermLabel : String
unknownTermLabel =
  "(unknown term)"

showDefinition : Model -> Definition -> String
showDefinition model definition =
  definition |> List.map (showSegment model) |> String.join ""

showSegment : Model -> Segment -> String
showSegment model segment =
  case segment of
    Text text ->
      text
    TermReference relatedTermRef ->
      relatedTermRef |> showRelatedTermName model

showRelatedTermName : Model -> Reference Term -> String
showRelatedTermName model relatedTermRef =
  relatedTermRef.get model.terms
  |> Maybe.map termInfo
  |> Maybe.map .name
  |> Maybe.withDefault unknownTermLabel

viewRelated : Address Action -> Model -> Reference TermView -> List (Reference TermView) -> Html
viewRelated address model parentTermViewRef related =
  let result =
        Html.div
          []
          (header ++ [relatedTermViews])
      header =
        if related |> List.isEmpty then
          []
        else
          [
            Html.div
              [Attributes.style [("font-style", "italic")]]
              [Html.text "where"]
          ]
      relatedTermViews =
        related |> viewTermViewRefs address model (Just parentTermViewRef)
  in result

viewCommandInput : Address Action -> Model -> Html
viewCommandInput address model =
  let result =
        Combobox.view comboboxAddress props model.commandInput
      comboboxAddress =
        updateCommandInput props |> Signal.forwardTo address
      props =
        {
          items =
            model |> getItems address,
          style =
            Combobox.ContentEditable,
          htmlAttributes = [
            termViewStyle "inline"
          ]
        }
  in result

getItems : Address Action -> Model -> List Combobox.Item
getItems address model =
  let result =
        open ++ create
      create =
        if model.terms.rows |> Dict.values |> List.any (\(Term termInfo) ->
          termInfo.name == model.commandInput.inputText
        ) then
          []
        else
          [
            {
              label =
                "Create “" ++ model.commandInput.inputText ++ "”",
              task =
                createTerm model.commandInput.inputText |> action |> Signal.send address
            }
          ]
      open =
        model.terms.rows |> Dict.toList |> List.map (\(id, Term termInfo) ->
          {
            label =
              "Open “" ++ termInfo.name ++ "”",
            task =
              openTermView Nothing (id |> reference) |> action |> Signal.send address
          }
        )
  in result
