port module WebSocket exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- ports


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- model


type alias Model =
    { streamTime : Bool
    , time : String
    }


initModel : Model
initModel =
    { streamTime = False
    , time = ""
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- update


type Msg
    = ToggleStreaming
    | Time String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleStreaming ->
            let
                newModel =
                    { model | streamTime = not model.streamTime }

                message =
                    if newModel.streamTime then
                        "start"

                    else
                        "stop"
            in
            ( newModel, sendMessage message )

        Time time ->
            ( { model | time = time }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    let
        toggleLabel =
            if model.streamTime then
                "Stop"

            else
                "Start"
    in
    div []
        [ button [ onClick ToggleStreaming ] [ text toggleLabel ]
        , br [] []
        , text model.time
        ]



-- subscriptions


decodeTime : String -> Msg
decodeTime message =
    decodeString (at [ "time" ] string) message
        |> Result.withDefault "Error Decoding Time"
        |> Time


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.streamTime then
        messageReceiver decodeTime

    else
        Sub.none
