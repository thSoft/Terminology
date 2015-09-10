module Terminology.Update where

import Terminology.Table exposing (..)
import Terminology.Model exposing (..)

type alias Update =
  Model -> Model

noOp : Update
noOp model =
  model

openTerm : Reference TermView -> TermView -> Reference Term -> Update
openTerm parentTermViewRef (TermView parentTermViewInfo) relatedTermRef model =
  let result =
        { model |
          termViews <-
            termViewInserted.newTable |> update parentTermViewRef.id (Just updatedParentTermView)
        }
      termViewInserted =
        model.termViews |> insert (termView relatedTermRef)
      updatedParentTermView =
        TermView { parentTermViewInfo |
          related <-
            parentTermViewInfo.related ++ [termViewInserted.newReference]
        }
  in result

closeTermView : Maybe (Reference TermView) -> Reference TermView -> Update
closeTermView maybeParentTermViewRef termViewRef model =
  case maybeParentTermViewRef of
    Nothing ->
      let result =
            { model |
              rootTermViews <-
                updatedRootTermViews
            }
          updatedRootTermViews =
            model.rootTermViews |> remove termViewRef
      in result
    Just parentTermViewRef ->
      let result =
            { model |
              termViews <-
                updatedTermViews
            }
          updatedTermViews =
            model.termViews |> update parentTermViewRef.id updatedParentTermView
          updatedParentTermView =
            parentTermViewRef.get model.termViews |> Maybe.map updateParentTermView
          updateParentTermView (TermView parentTermView) =
            TermView { parentTermView |
              related <-
                parentTermView.related |> remove termViewRef
            }
      in result

remove : a -> List a -> List a
remove element list =
  list |> List.filter (\elem -> elem /= element)
