module Terminology.Table where

import Dict exposing (Dict)

type alias Id =
  Int

type alias Table a =
  {
    rows: Dict Id a,
    nextId: Id
  }

empty : Table a
empty =
  {
    rows =
      Dict.empty,
    nextId =
      0
  }

insert : a -> Table a -> InsertionResult a
insert row table =
  {
    newTable =
      {
        rows =
          table.rows |> Dict.insert table.nextId row,
        nextId =
          table.nextId + 1
      },
    newReference =
      table.nextId |> reference
  }

type alias InsertionResult a =
  {
    newTable: Table a,
    newReference: Reference a
  }

update : Id -> Maybe a -> Table a -> Table a
update id maybeRow table =
  { table |
    rows <-
      table.rows |> Dict.update id (always maybeRow)
  }

type alias Reference a =
  {
    id: Id,
    get: Table a -> Maybe a
  }

reference : Id -> Reference a
reference id =
  {
    id =
      id,
    get table =
      table |> .rows |> Dict.get id
  }
