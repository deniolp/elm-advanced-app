module Jokes exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)



-- model


type alias Response =
    { id : Int
    , joke : String
    , categories : List String
    }


type alias Model =
    String


initModel : Model
initModel =
    "Finding a joke..."


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, randomJoke )


responseDecoder : Decoder Response
responseDecoder =
    map3 Response
        (field "id" int)
        (field "joke" string)
        (field "categories" (list string))
        |> at [ "value" ]


randomJoke : Cmd Msg
randomJoke =
    Http.get
        { url = "http://api.icndb.com/jokes/random"
        , expect = Http.expectJson Joke responseDecoder
        }



-- update


type Msg
    = Joke (Result Http.Error Response)
    | NewJoke


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Joke (Ok response) ->
            ( Debug.toString response.id ++ " " ++ response.joke, Cmd.none )

        Joke (Err err) ->
            ( Debug.toString err, Cmd.none )

        NewJoke ->
            ( "fetching new joke", randomJoke )



-- view


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model ]
        , button
            [ onClick NewJoke
            ]
            [ text "Click" ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
