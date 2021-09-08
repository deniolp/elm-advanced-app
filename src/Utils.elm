module Utils exposing (..)

import Http exposing (..)
import Json.Decode as JD


type ErrorDetailed body
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata body
    | BadBody Http.Metadata body String


expectJsonCustom : (Result (ErrorDetailed String) ( Http.Metadata, String ) -> msg) -> JD.Decoder String -> Http.Expect msg
expectJsonCustom msg decoder =
    Http.expectStringResponse msg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata body)

                Http.GoodStatus_ metadata body ->
                    Result.mapError (BadBody metadata body) <|
                        Result.mapError JD.errorToString
                            (JD.decodeString (JD.map (\resp -> ( metadata, resp )) decoder) body)
