module Terminology.Update where

import Terminology.Table exposing (..)
import Terminology.Model exposing (..)

type alias Update =
  Model -> Model

noOp : Update
noOp model =
  model

openTerm : Id -> TermView -> Reference Term -> Update
openTerm parentTermViewId (TermView parentTermViewInfo) relatedTermRef model =
  let result =
        { model |
          termViews <-
            termViewInserted.newTable |> update parentTermViewId (Just updatedParentTermView)
        }
      termViewInserted =
        model.termViews |> insert (termView relatedTermRef)
      updatedParentTermView =
        TermView { parentTermViewInfo |
          related <-
            parentTermViewInfo.related ++ [newRelatedTermViewRef]
        }
      newRelatedTermViewRef =
        termViewInserted.newReference
  in result
