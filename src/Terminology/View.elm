module Terminology.View where

import String
import Dict exposing (Dict)
import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Terminology.Table exposing (..)
import Terminology.Model exposing (..)
import Terminology.Update exposing (..)

view : Address Update -> Model -> Html
view address model =
  Html.div
    [
      Attributes.style [
        ("font-family", "Baskerville, Georgia")
      ]
    ]
    [
      Html.h1 [] [Html.text "Terminology Editor"],
      model.rootTermViews |> viewTermViewRefs address model Nothing
    ]

viewTermViewRefs : Address Update -> Model -> Maybe (Reference TermView) -> List (Reference TermView) -> Html
viewTermViewRefs address model maybeParentTermViewRef termViewRefs =
  Html.dl
    []
    (termViewRefs |> List.map (viewTermViewRef address model maybeParentTermViewRef))

viewTermViewRef : Address Update -> Model -> Maybe (Reference TermView) -> Reference TermView -> Html
viewTermViewRef address model maybeParentTermViewRef termViewRef =
  termViewRef.get model.termViews
  |> Maybe.map (viewTermView address model maybeParentTermViewRef termViewRef)
  |> Maybe.withDefault (Html.div [] [Html.text "Referenced term view not found!"])

viewTermView : Address Update -> Model -> Maybe (Reference TermView) -> Reference TermView -> TermView -> Html
viewTermView address model maybeParentTermViewRef termViewRef (TermView termViewInfo) =
  termViewInfo.term.get model.terms
  |> Maybe.map (viewTerm address model maybeParentTermViewRef termViewRef (TermView termViewInfo))
  |> Maybe.withDefault (Html.div [] [Html.text "Referenced term not found!"])

viewTerm : Address Update -> Model -> Maybe (Reference TermView) -> Reference TermView -> TermView -> Term -> Html
viewTerm address model maybeParentTermViewRef termViewRef termView (Term termInfo) =
  let result =
        Html.div
          [
            Attributes.style [
              ("display", "table"),
              ("padding", "5px"),
              ("margin", "5px"),
              ("border-radius", "2px"),
              ("box-shadow", "0px 0px 2px 2px #aaaaaa")
            ]
          ]
          [
            close,
            name,
            definition,
            relatedTerms
          ]
      name =
        termInfo.name |> viewName
      definition =
        termInfo.definition |> viewDefinition address model termViewRef termView
      relatedTerms =
        termView |> termViewInfo |> .related |> viewRelated address model termViewRef
      close =
        Html.img
          [
            Attributes.src "https://upload.wikimedia.org/wikipedia/commons/f/f8/Tooltip-CloseButton.png",
            Events.onClick address (closeTermView maybeParentTermViewRef termViewRef),
            Attributes.style [
              ("float", "right")
            ]
          ]
          []
  in result

viewName : String -> Html
viewName name =
  Html.dt
    [
      Attributes.style [
        ("font-weight", "bold")
      ]
    ]
    [Html.text name]

viewDefinition : Address Update -> Model -> Reference TermView -> TermView -> Definition -> Html
viewDefinition address model termViewRef termView definition =
  Html.dt
    []
    (definition |> List.map (viewSegment address model termViewRef termView))

viewSegment : Address Update -> Model -> Reference TermView -> TermView -> Segment -> Html
viewSegment address model termViewRef termView segment =
  case segment of
    Text text ->
      Html.text text
    TermReference relatedTermRef ->
      let result =
            Html.a
              [Events.onClick address (openTerm termViewRef termView relatedTermRef)]
              [
                Html.abbr
                  [Attributes.title relatedTermDefinition]
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
  |> Maybe.withDefault "(not found)"

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
  |> Maybe.withDefault "(not found)"

viewRelated : Address Update -> Model -> Reference TermView -> List (Reference TermView) -> Html
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
