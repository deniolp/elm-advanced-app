module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LeaderBoard
import Login



-- model


type alias Model =
    { page : Page
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    }


type Page
    = LoginPage
    | LeaderBoardPage


initModel : Model
initModel =
    { page = LeaderBoardPage
    , leaderBoard = LeaderBoard.initModel
    , login = Login.initModel
    }



-- update


type Msg
    = ChangePage Page
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePage page ->
            { model
                | page = page
            }

        LeaderBoardMsg lbMsg ->
            { model
                | leaderBoard =
                    LeaderBoard.update lbMsg model.leaderBoard
            }

        LoginMsg loginMsg ->
            { model
                | login =
                    Login.update loginMsg model.login
            }



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LeaderBoardPage ->
                    Html.map LeaderBoardMsg
                        (LeaderBoard.view model.leaderBoard)

                LoginPage ->
                    Html.map LoginMsg
                        (Login.view model.login)
    in
    div []
        [ div []
            [ a
                [ href "#"
                , onClick (ChangePage LeaderBoardPage)
                ]
                [ text "LeaderBoard" ]
            , span [] [ text " | " ]
            , a
                [ href "#"
                , onClick (ChangePage LoginPage)
                ]
                [ text "Login" ]
            , hr [] []
            , page
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, update = update, view = view }
