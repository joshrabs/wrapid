module Client.Generic.WebForm.Types exposing (..)

import Dict exposing (Dict)

-- type alias Field =
--   {id: FieldId
--   ,label: FieldLabel
--   ,input: FieldInput
--   ,category: FieldCategory
--   }


type FieldCategory = Name | Address | Doctor | Unknown
type FieldInputType = TextInput | DropDown (List DropDownOption)

type alias DropDownOption = String
type alias FieldLabel = String
type alias FieldInput = String
type alias FieldId = String

type alias FieldCategoryMap = Dict FieldId FieldCategory
type alias FieldInputTypeMap = Dict FieldId FieldInputType
type alias FieldLabelMap = Dict FieldId FieldLabel
