module Lessons.Decoder.SimpleDecoder exposing (..)

import Debug exposing (toString)
import Html exposing (..)
import Json.Decode exposing (..)


json : String
json =
    """
  {
    "type":"success",
    "value":{
      "id":496,
      "joke":"Chuck Norris is very strange",
      "categories":[
        "nerdy"
      ]
    }
  }
"""


decoder : Decoder String
decoder =
    at [ "value", "joke" ] string


jokeResult : Result Error String
jokeResult =
    decodeString decoder json


main : Html msg
main =
    case jokeResult of
        Ok joke ->
            text joke

        Err err ->
            text (toString err)
