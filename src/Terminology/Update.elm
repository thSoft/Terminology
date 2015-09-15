module Terminology.Update where

import Terminology.Table exposing (..)
import Terminology.Model exposing (..)
import Terminology.Combobox as Combobox

type alias Update =
  Model -> Model

updateCommandInput : Combobox.Props -> Combobox.Update -> Update
updateCommandInput props comboboxUpdate model =
  { model |
    commandInput <-
      model.commandInput |> comboboxUpdate props
  }

openTermView : Maybe (Reference TermView) -> Reference Term -> Update
openTermView maybeParentTermViewRef relatedTermRef model =
  let result =
        case maybeParentTermViewRef of
          Nothing ->
            let result =
                  { model |
                    termViews <-
                      termViewInserted.newTable,
                    rootTermViews <-
                      model.rootTermViews ++ [termViewInserted.newReference],
                    commandInput <-
                      { oldCommandInput |
                        inputText <-
                          ""
                      }
                  }
                oldCommandInput =
                  model.commandInput
            in result
          Just parentTermViewRef ->
            case parentTermViewRef.get model.termViews of
              Nothing ->
                model
              Just (TermView parentTermViewInfo) ->
                let result =
                      { model |
                        termViews <-
                          termViewInserted.newTable |> update parentTermViewRef.id (Just updatedParentTermView)
                      }
                    updatedParentTermView =
                      TermView { parentTermViewInfo |
                        related <-
                          parentTermViewInfo.related ++ [termViewInserted.newReference]
                      }
                in result
      termViewInserted =
        model.termViews |> insert (termView relatedTermRef)
  in result

closeTermView : Maybe (Reference TermView) -> Reference TermView -> Update
closeTermView maybeParentTermViewRef termViewRef model =
  case maybeParentTermViewRef of
    Nothing ->
      { model |
        rootTermViews <-
          model.rootTermViews |> remove termViewRef
      }
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
