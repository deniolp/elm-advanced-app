module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import Url



-- main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- model


type alias Model =
    { page : Page
    , key : Nav.Key
    , url : Url.Url

    -- , leaderBoard : LeaderBoard.Model
    -- , login : Login.Model
    }


type Page
    = NotFound
    | LeaderBoardPage
    | LoginPage
    | AddRunnerPage


initModel : Url.Url -> Nav.Key -> Model
initModel url key =
    { key = key
    , url = url
    , page = url |> fragmentToPage

    -- , leaderBoard = LeaderBoard.initModel
    -- , login = Login.initModel
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initModel url key, Cmd.none )



-- update


type Msg
    = ChangePage Page
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url



-- | LeaderBoardMsg LeaderBoard.Msg
-- | LoginMsg Login.Msg


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

        ChangePage page ->
            Debug.log "Updated model"
                ( { model
                    | page = page
                  }
                , Cmd.none
                )


pageToFragment : Page -> String
pageToFragment page =
    case page of
        LeaderBoardPage ->
            "#"

        AddRunnerPage ->
            "#add"

        LoginPage ->
            "#login"

        NotFound ->
            "#not-found"


fragmentToPage : Url.Url -> Page
fragmentToPage url =
    case withDefault "" url.fragment of
        "" ->
            LeaderBoardPage

        "add" ->
            AddRunnerPage

        "login" ->
            LoginPage

        _ ->
            NotFound



-- LeaderBoardMsg lbMsg ->
--     { model
--         | leaderBoard =
--             LeaderBoard.update lbMsg model.leaderBoard
--     }
-- LoginMsg loginMsg ->
--     { model
--         | login =
--             Login.update loginMsg model.login
--     }
-- view


view : Model -> Browser.Document Msg
view model =
    { title = "First Try!"
    , body =
        [ let
            page =
                case model.page of
                    NotFound ->
                        div [ class "main" ]
                            [ h1 [] [ text "Page not found" ]
                            ]

                    LeaderBoardPage ->
                        text "k"

                    -- Html.map LeaderBoardMsg
                    --     (LeaderBoard.view model.leaderBoard)
                    LoginPage ->
                        text "j"

                    -- Html.map LoginMsg
                    --     (Login.view model.login)
                    AddRunnerPage ->
                        text "l"
          in
          div []
            [ pageHeader model
            , page
            ]
        ]
    }


pageHeader : Model -> Html Msg
pageHeader _ =
    header []
        [ a [ href "#/" ] [ text "Race Results" ]
        , ul []
            [ li []
                [ a [ href "#" ] [ text "Link" ] ]
            ]
        , ul []
            [ li []
                [ a [ href "#" ] [ text "Login" ] ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
