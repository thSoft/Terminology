module Terminology.Model where

import Table exposing (..)
import Combobox

type alias Model =
  {
    terms: Table Term,
    termViews: Table TermView,
    rootTermViews: List (Reference TermView),
    commandInput: Combobox.State
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

term : String -> Term
term name =
  Term {
    name = name,
    definition = []
  }

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

termView : Reference Term -> TermView
termView termRef =
  TermView {
    term = termRef,
    related = []
  }
