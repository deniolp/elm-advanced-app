module SPA.Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Main exposing (Page(..))
import Maybe exposing (withDefault)
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
    , page : Page
    }


type Page
    = LeaderBoard
    | AddRunner
    | Login
    | NotFound


initModel : Url.Url -> Nav.Key -> Model
initModel url key =
    { key = key
    , url = url
    , page = url |> fragmentToPage
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initModel url key, Cmd.none )



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
                    let
                        newModel =
                            { model
                                | page = url |> fragmentToPage
                            }
                    in
                    ( newModel
                    , Nav.pushUrl model.key (newModel.page |> pageToFragment)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                newModel =
                    { model
                        | page = url |> fragmentToPage
                        , url = url
                    }
            in
            ( newModel, Cmd.none )


pageToFragment : Page -> String
pageToFragment page =
    case page of
        LeaderBoard ->
            "#"

        AddRunner ->
            "#add"

        Login ->
            "#login"

        NotFound ->
            "#not-found"


fragmentToPage : Url.Url -> Page
fragmentToPage url =
    case withDefault "" url.fragment of
        "" ->
            LeaderBoard

        "add" ->
            AddRunner

        "login" ->
            Login

        _ ->
            NotFound



--view


menu : Model -> Html Msg
menu _ =
    header []
        [ a [ href "#" ]
            [ text "LeaderBoard" ]
        , text " | "
        , a [ href "#add" ]
            [ text "AddRunner" ]
        , text " | "
        , a [ href "#login" ]
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
    { title = "First Try!"
    , body =
        [ let
            page =
                case model.page of
                    LeaderBoard ->
                        viewPage "LeaderBoard Page"

                    AddRunner ->
                        viewPage "AddRunner Page"

                    Login ->
                        viewPage "Login Page"

                    _ ->
                        viewPage "Not Found Page"
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
