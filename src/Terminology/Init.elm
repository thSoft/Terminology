module Terminology.Init where

import Effects exposing (Effects)
import Table exposing (..)
import Combobox
import Terminology.Model exposing (..)
import Terminology.Update exposing (..)

init : (Model, Effects Action)
init =
  let result =
        {
          terms =
            terms,
          termViews =
            fooViewInserted.newTable,
          rootTermViews =
            [],
          commandInput =
            Combobox.initialState
        }
      terms =
        fooInserted.newTable
      bazInserted =
        empty |> insert baz
      barInserted =
        bazInserted.newTable |> insert (bar bazInserted.newReference)
      fooInserted =
        barInserted.newTable |> insert (foo barInserted.newReference)
      fooViewInserted =
        empty |> insert (termView fooInserted.newReference)
  in (result, Effects.none)

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
