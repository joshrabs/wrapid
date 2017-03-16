module Client.Utilities.DateTime exposing (..)

import Date exposing (Date)
import Date.Extra.Format exposing (format)
import Date.Extra.Config.Config_en_us exposing (config)

frmtDate: Maybe Date -> String
frmtDate date =
    date
      |> Maybe.map (Date.Extra.Format.format Date.Extra.Config.Config_en_us.config "%Y-%m-%d")
      |> Maybe.withDefault ""
