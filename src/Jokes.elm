module Jokes exposing (..)

import Browser
import Html exposing (..)
import Http



-- model


type alias Model =
    String


initModel : Model
initModel =
    "Finding a joke..."


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, randomJoke )


randomJoke : Cmd Msg
randomJoke =
    Http.get
        { url = "http://api.icndb.com/jokes/random"
        , expect = Http.expectString Joke
        }



-- update


type Msg
    = Joke (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Joke (Ok joke) ->
            ( joke, Cmd.none )

        Joke (Err err) ->
            ( Debug.toString err, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [] [ text model ]



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
