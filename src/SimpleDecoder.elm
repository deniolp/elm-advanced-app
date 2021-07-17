module SimpleDecoder exposing (..)

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


jokeRsult : Result Error String
jokeRsult =
    decodeString decoder json


main : Html msg
main =
    case jokeRsult of
        Ok joke ->
            text joke

        Err err ->
            text (toString err)
