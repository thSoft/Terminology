module Terminology.Update where

import Effects exposing (Effects)
import Table exposing (..)
import Combobox
import Terminology.Model exposing (..)

type alias Action =
  Model -> ActionResult

type ActionResult =
  ActionResult {
    updatedModel: Model,
    effects: Effects Action
  }

type alias UpdateModel =
  Model -> Model

action : UpdateModel -> Action
action updateModel model =
  ActionResult {
    updatedModel =
      model |> updateModel,
    effects =
      Effects.none
  }

updateCommandInput : Combobox.Props -> Combobox.Action -> Action
updateCommandInput comboboxProps comboboxAction model =
  let (updatedCommandInput, comboboxEffects) =
        model.commandInput |> Combobox.update comboboxAction comboboxProps
      updatedModel =
        { model | commandInput <- updatedCommandInput }
      effects =
        comboboxEffects |> Effects.map (\nextComboboxAction ->
          nextComboboxAction |> updateCommandInput comboboxProps
        )
  in
    ActionResult {
      updatedModel = updatedModel,
      effects = effects
    }

update : Action -> Model -> (Model, Effects Action)
update action model =
  let (ActionResult { updatedModel, effects }) =
        model |> action
  in (updatedModel, effects)

createTerm : String -> UpdateModel
createTerm name model =
  let result =
        modelWithTerm |> openTermView Nothing termInserted.newReference
      modelWithTerm =
        { model |
          terms <-
            termInserted.newTable
        }
      termInserted =
        model.terms |> insert (term name)
  in result

deleteTerm : Maybe (Reference TermView) -> Reference TermView -> UpdateModel
deleteTerm maybeParentTermViewRef termViewRef model =
  case termViewRef.get model.termViews of
    Nothing ->
      model
    Just (TermView termViewInfo) ->
      let result =
            modelWithoutTerm |> closeTermView maybeParentTermViewRef termViewRef
          modelWithoutTerm =
            { model |
              terms <-
                model.terms |> Table.remove termViewInfo.term
            }
      in result

openTermView : Maybe (Reference TermView) -> Reference Term -> UpdateModel
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
                          termViewInserted.newTable |> Table.update parentTermViewRef.id (Just updatedParentTermView)
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

closeTermView : Maybe (Reference TermView) -> Reference TermView -> UpdateModel
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
            model.termViews |> Table.update parentTermViewRef.id updatedParentTermView
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
