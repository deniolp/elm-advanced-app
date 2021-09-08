module Login exposing (..)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Utils exposing (ErrorDetailed, expectJsonCustom)



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
    | LoginResponse (Result (ErrorDetailed String) ( Http.Metadata, String ))


responseDecoder : JD.Decoder String
responseDecoder =
    JD.field "token" JD.string


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
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
                        , expect = expectJsonCustom LoginResponse responseDecoder
                        }
            in
            ( model, cmd, Nothing )

        Error error ->
            ( { model | error = Just error }, Cmd.none, Nothing )

        LoginResponse (Ok data) ->
            ( initModel, Nav.load "#", Just (Tuple.second data) )

        LoginResponse (Err err) ->
            let
                errorMsg =
                    case err of
                        Utils.BadStatus meta body ->
                            case meta.statusCode of
                                401 ->
                                    body

                                _ ->
                                    meta.statusText

                        Utils.BadBody _ _ error ->
                            error

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
