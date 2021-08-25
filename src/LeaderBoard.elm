module LeaderBoard exposing (..)

import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- model


type alias Model =
    { error : Maybe String
    , runners : List Runner
    , query : String
    , active : Bool
    }


type alias Runner =
    { id : Int
    , name : String
    , location : String
    , age : Int
    , bib : Int
    , estimatedDistance : Float
    , lastMarkerDistance : Float
    , lastMarkerTime : Float
    , pace : Float
    }


tempRunners : List Runner
tempRunners =
    [ Runner 1 "Denis Popov" "Kazan" 42 1234 0 1 1463154945381 0
    , Runner 2 "Miko Lamus" "Kazan" 41 1238 0 1 1463154945382 0
    ]


initModel : Model
initModel =
    { error = Nothing
    , runners = tempRunners
    , query = ""
    , active = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = SearchInput String
    | Search


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            Debug.log "Input query updated model"
                ( { model | query = query }, Cmd.none )

        Search ->
            ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , searchForm model.query
        , runners model
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "x" ]
                ]


searchForm : String -> Html Msg
searchForm query =
    Html.form [ onSubmit Search ]
        [ input
            [ type_ "text"
            , placeholder "Search for runner"
            , value query
            , onInput SearchInput
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]


runners : Model -> Html Msg
runners model =
    model.runners
        |> List.map runner
        |> tbody []
        |> (\t -> [ runnersHeader, t ])
        |> table []


runner : Runner -> Html Msg
runner { name, location, age, bib, estimatedDistance } =
    tr []
        [ td [] [ text name ]
        , td [] [ text location ]
        , td [] [ text (toString age) ]
        , td [] [ text (toString bib) ]
        , td [] [ text "1 mi @ 08:30AM (TODO)" ]
        , td [] [ text (toString estimatedDistance) ]
        ]


runnersHeader : Html Msg
runnersHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "From" ]
            , th [] [ text "Age" ]
            , th [] [ text "Bib" ]
            , th [] [ text "Last Marker" ]
            , th [] [ text "Est. miles" ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
