module Client.Generic.WebForm.Utils exposing (..)

import Client.Generic.WebForm.Types exposing (..)

import Dict exposing (fromList, toList, get)

masterFieldLabelMap: FieldLabelMap
masterFieldLabelMap =
  let
      labelList =
        [("firstName", "First Name")
        ,("lastName", "Last Name")
        ]
  in
      fromList labelList

defaultFieldLabel: FieldLabelMap -> FieldId -> FieldLabel
defaultFieldLabel fieldLabelMap fieldId =
    Dict.get fieldId fieldLabelMap
      |> Maybe.withDefault "meow"

getFieldLabel = defaultFieldLabel masterFieldLabelMap

fieldCategoryMap: FieldCategoryMap
fieldCategoryMap =
  let
      categoryList =
        [("firstName", Name)
        ,("lastName", Name)
        ,("MI", Name)
        ]
  in
    Dict.fromList categoryList


getFieldCategory = defaultFieldCategory fieldCategoryMap

defaultFieldCategory : FieldCategoryMap -> FieldId -> FieldCategory
defaultFieldCategory categoryMap fieldId =
  Dict.get fieldId categoryMap
    |> Maybe.withDefault Unknown



masterFieldInputTypeMap: FieldInputTypeMap
masterFieldInputTypeMap =
  let
      mapList =
        [("firstName", TextInput)
        ,("lastName", TextInput)
        ,("MI", TextInput)
        ]
  in
      fromList mapList

getFieldInputType = defaultFieldInputType masterFieldInputTypeMap

defaultFieldInputType: FieldInputTypeMap -> FieldId -> FieldInputType
defaultFieldInputType inputTypeMap fieldId =
  Dict.get fieldId inputTypeMap
    |> Maybe.withDefault TextInput
