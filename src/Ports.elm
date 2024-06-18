port module Ports exposing (..)

import Date exposing (Date)
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time
import Types exposing (..)



-- Ports --


port saveCache : Encode.Value -> Cmd msg


port clearCache : Encode.Value -> Cmd msg



-- port cache : Encode.Value -> Cmd msg
-- port fetch : (Encode.Value -> msg) -> Sub msg
-- Main --
