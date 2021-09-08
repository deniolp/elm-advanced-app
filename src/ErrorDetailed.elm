module ErrorDetailed exposing (..)

import Http exposing (..)


type ErrorDetailed body
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata body
    | BadBody Http.Metadata body String
