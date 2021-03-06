port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LeaderBoard
import Login
import Maybe exposing (withDefault)
import Runner
import Url



-- main


main : Program Flags Model Msg
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
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    , runner : Runner.Model
    , token : Maybe String
    , loggedIn : Bool
    }


type alias Flags =
    { token : Maybe String
    }


type Page
    = NotFound
    | LeaderBoardPage
    | LoginPage
    | AddRunnerPage


authPages : List Page
authPages =
    [ AddRunnerPage ]


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        page =
            url |> fragmentToPage

        loggedIn =
            flags.token /= Nothing

        ( updatedPage, cmd ) =
            authRedirect page loggedIn key

        ( leaderBoardInitModel, leaderBoardCmd ) =
            LeaderBoard.init

        ( loginInitModel, loginCmd ) =
            Login.init

        ( runnerInitModel, runnerCmd ) =
            Runner.init

        initModel =
            { key = key
            , url = url
            , page = updatedPage
            , leaderBoard = leaderBoardInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            , token = flags.token
            , loggedIn = loggedIn
            }

        cmds =
            Cmd.batch
                [ Cmd.map LeaderBoardMsg leaderBoardCmd
                , Cmd.map LoginMsg loginCmd
                , Cmd.map RunnerMsg runnerCmd
                , cmd
                ]
    in
    ( initModel, cmds )



-- update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg
    | RunnerMsg Runner.Msg
    | Logout


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
                ( updatedPage, cmd ) =
                    authRedirect (url |> fragmentToPage) model.loggedIn model.key

                newModel =
                    { model
                        | page = updatedPage
                        , url = url
                    }
            in
            ( newModel, cmd )

        LeaderBoardMsg lbMsg ->
            let
                ( leaderBoardModel, cmd ) =
                    LeaderBoard.update lbMsg model.leaderBoard
            in
            ( { model | leaderBoard = leaderBoardModel }
            , Cmd.map LeaderBoardMsg cmd
            )

        LoginMsg loginMsg ->
            let
                ( loginModel, cmd, token ) =
                    Login.update loginMsg model.login

                loggedIn =
                    token /= Nothing

                saveTokenCmd =
                    case token of
                        Just jwt ->
                            saveToken jwt

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | login = loginModel
                , token = token
                , loggedIn = loggedIn
              }
            , Cmd.batch
                [ Cmd.map LoginMsg cmd
                , saveTokenCmd
                ]
            )

        RunnerMsg runnerMsg ->
            let
                ( runnerModel, cmd ) =
                    Runner.update (Maybe.withDefault "" model.token) runnerMsg model.runner
            in
            ( { model | runner = runnerModel }
            , Cmd.map RunnerMsg cmd
            )

        Logout ->
            ( { model
                | loggedIn = False
                , token = Nothing
              }
            , deleteToken ()
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


authForPage : Page -> Bool -> Bool
authForPage page loggedIn =
    loggedIn || not (List.member page authPages)


authRedirect : Page -> Bool -> Nav.Key -> ( Page, Cmd Msg )
authRedirect page loggedIn key =
    if authForPage page loggedIn then
        ( page, Cmd.none )

    else
        ( LoginPage, Nav.pushUrl key (LoginPage |> pageToFragment) )



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Race results"
    , body =
        [ let
            page =
                case model.page of
                    NotFound ->
                        div [ class "main" ]
                            [ h1 [] [ text "Page not found" ]
                            ]

                    LeaderBoardPage ->
                        Html.map LeaderBoardMsg
                            (LeaderBoard.view model.leaderBoard)

                    LoginPage ->
                        Html.map LoginMsg
                            (Login.view model.login)

                    AddRunnerPage ->
                        Html.map RunnerMsg
                            (Runner.view model.runner)
          in
          div []
            [ pageHeader model
            , page
            ]
        ]
    }


authHeaderView : Model -> Html Msg
authHeaderView { loggedIn } =
    if loggedIn then
        a [ onClick Logout ] [ text "Logout" ]

    else
        a [ href "#login" ] [ text "Login" ]


addRunnerLinkView : Model -> Html Msg
addRunnerLinkView { loggedIn } =
    if loggedIn then
        a [ href "#add" ] [ text "Add Runner" ]

    else
        text ""


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ href "#" ] [ text "Race Results" ]
        , ul []
            [ li []
                [ addRunnerLinkView model ]
            ]
        , ul []
            [ li []
                [ authHeaderView model ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        leaderBoardSub =
            LeaderBoard.subscriptions model.leaderBoard

        loginSub =
            Login.subscriptions model.login

        runnerSub =
            Runner.subscriptions model.runner
    in
    Sub.batch
        [ Sub.map LeaderBoardMsg leaderBoardSub
        , Sub.map LoginMsg loginSub
        , Sub.map RunnerMsg runnerSub
        ]



-- ports


port saveToken : String -> Cmd msg


port deleteToken : () -> Cmd msg
