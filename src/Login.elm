module Login exposing (..)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (field)
import Json.Encode as JE



-- model


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Error String
    | LoginResponse (Result Http.Error String)


responseDecoder : JD.Decoder String
responseDecoder =
    field "token" JD.string


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        UsernameInput username ->
            Debug.log "Input username updated model"
                ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
            Debug.log "Input password updated model"
                ( { model | password = password }, Cmd.none, Nothing )

        Submit ->
            let
                cmd =
                    Http.post
                        { url = "http://localhost:5000/authenticate"
                        , body =
                            JE.object
                                [ ( "username", JE.string model.username )
                                , ( "password", JE.string model.password )
                                ]
                                |> JE.encode 4
                                |> Http.stringBody "application/json"
                        , expect = Http.expectJson LoginResponse responseDecoder
                        }
            in
            ( model, cmd, Nothing )

        Error error ->
            Debug.log "Error updated model"
                ( { model | error = Just error }, Cmd.none, Nothing )

        LoginResponse (Ok token) ->
            ( initModel, Nav.load "#", Just token )

        LoginResponse (Err err) ->
            let
                errorMsg =
                    case err of
                        Http.BadStatus resp ->
                            case resp of
                                401 ->
                                    "resp.body"

                                _ ->
                                    "resp.status.message"

                        _ ->
                            "Login Error"
            in
            ( { model | error = Just errorMsg }, Cmd.none, Nothing )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , loginForm model
        ]


loginForm : Model -> Html Msg
loginForm model =
    Html.form [ class "add-runner", onSubmit Submit ]
        [ fieldset []
            [ legend [] [ text "Login" ]
            , div []
                [ label [] [ text "User Name" ]
                , input
                    [ type_ "text"
                    , value model.username
                    , onInput UsernameInput
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , value model.password
                    , onInput PasswordInput
                    ]
                    []
                ]
            , div []
                [ label [] []
                , button
                    [ type_ "submit" ]
                    [ text "Login" ]
                ]
            ]
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ] [ text msg ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
