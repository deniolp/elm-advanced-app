module Main exposing (..)

-- import LeaderBoard
-- import Login

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- model


type alias Model =
    { page : Page

    -- , leaderBoard : LeaderBoard.Model
    -- , login : Login.Model
    }


type Page
    = NotFound



-- | LeaderBoardPage
-- | LoginPage


initModel : Model
initModel =
    { page = NotFound

    -- , leaderBoard = LeaderBoard.initModel
    -- , login = Login.initModel
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- update


type Msg
    = ChangePage Page



-- | LeaderBoardMsg LeaderBoard.Msg
-- | LoginMsg Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            Debug.log "Updated model"
                ( { model
                    | page = page
                  }
                , Cmd.none
                )



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


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                NotFound ->
                    div [ class "main" ]
                        [ h1 [] [ text "Page not found" ]
                        ]

        -- LeaderBoardPage ->
        --     Html.map LeaderBoardMsg
        --         (LeaderBoard.view model.leaderBoard)
        -- LoginPage ->
        --     Html.map LoginMsg
        --         (Login.view model.login)
    in
    div []
        [ pageHeader model
        , page
        ]


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
