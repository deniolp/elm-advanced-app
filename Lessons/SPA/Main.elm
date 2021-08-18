module SPA.Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url



-- main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url, Cmd.none )



--update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            Debug.log "Updated model"
                ( { model | url = url }
                , Cmd.none
                )



--view


menu : Model -> Html Msg
menu _ =
    header []
        [ a [ href "/leader-board" ]
            [ text "LeaderBoard" ]
        , text " | "
        , a [ href "/add-runner" ]
            [ text "AddRunner" ]
        , text " | "
        , a [ href "/login" ]
            [ text "Login" ]
        ]


viewPage : String -> Html Msg
viewPage pageDescription =
    div []
        [ h3 [] [ text pageDescription ]
        , p [] [ text <| "TODO: make " ++ pageDescription ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ let
            page =
                case model.url.path of
                    "/leader-board" ->
                        viewPage "LeaderBoard Page"

                    "/add-runner" ->
                        viewPage "AddRunner Page"

                    "/login" ->
                        viewPage "Login Page"

                    _ ->
                        viewPage "LeaderBoard Page"
          in
          div []
            [ menu model
            , hr [] []
            , page
            ]
        ]
    }



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
