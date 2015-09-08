module Terminology.Model where

import Terminology.Table exposing (..)

type alias Model =
  {
    terms: Table Term,
    termViews: Table TermView,
    rootTermViews: List (Reference TermView)
  }

type Term =
  Term TermInfo

type alias TermInfo =
  {
    name: String,
    definition: Definition
  }

termInfo : Term -> TermInfo
termInfo (Term termInfo) =
  termInfo

type alias Definition =
  List Segment

type Segment =
  Text String |
  TermReference (Reference Term)

type TermView =
  TermView TermViewInfo

type alias TermViewInfo =
  {
    term: Reference Term,
    related: List (Reference TermView)
  }

termViewInfo : TermView -> TermViewInfo
termViewInfo (TermView termViewInfo) =
  termViewInfo

initialModel : Model
initialModel =
  let result =
        {
          terms =
            fooInserted.newTable,
          termViews =
            fooViewInserted.newTable,
          rootTermViews =
            [fooViewInserted.newReference]
        }
      bazInserted = empty |> insert baz
      barInserted = bazInserted.newTable |> insert (bar bazInserted.newReference)
      fooInserted = barInserted.newTable |> insert (foo barInserted.newReference)
      fooViewInserted = empty |> insert (termView fooInserted.newReference)
  in result

foo : Reference Term -> Term
foo barRef =
  Term {
    name = "foo",
    definition = [
      Text "A form of ",
      TermReference barRef,
      Text "."
    ]
  }

bar : Reference Term -> Term
bar bazRef =
  Term {
    name = "bar",
    definition = [
      Text "An alternative to ",
      TermReference bazRef,
      Text "."
    ]
  }

baz : Term
baz =
  Term {
    name = "baz",
    definition = [
      Text "Trivial."
    ]
  }

termView : Reference Term -> TermView
termView termRef =
  TermView {
    term = termRef,
    related = []
  }
