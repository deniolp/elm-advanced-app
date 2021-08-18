module SPA.Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)



-- model


type alias Model =
    { page : Page
    }


type Page
    = LeaderBoard
    | AddRunner
    | Login
    | NotFound


initModel : Model
initModel =
    { page = LeaderBoard
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



--update


type Msg
    = Navigate Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Cmd.none )



--view


menu : Model -> Html Msg
menu _ =
    header []
        [ a [ onClick (Navigate LeaderBoard) ]
            [ text "LeaderBoard" ]
        , text " | "
        , a [ onClick (Navigate AddRunner) ]
            [ text "AddRunner" ]
        , text " | "
        , a [ onClick (Navigate Login) ]
            [ text "Login" ]
        ]


viewPage : String -> Html Msg
viewPage pageDescription =
    div []
        [ h3 [] [ text pageDescription ]
        , p [] [ text <| "TODO: make " ++ pageDescription ]
        ]


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LeaderBoard ->
                    viewPage "LeaderBoard Page"

                AddRunner ->
                    viewPage "AddRunner Page"

                Login ->
                    viewPage "Login Page"

                NotFound ->
                    viewPage "NotFound Page"
    in
    div []
        [ menu model
        , hr [] []
        , page
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
