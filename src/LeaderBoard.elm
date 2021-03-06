port module LeaderBoard exposing (..)

import Debug exposing (toString)
import Flip exposing (flip)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Task
import Time



-- ports


port listenRunnersMsg : String -> Cmd msg


port listen : (String -> msg) -> Sub msg



-- model


type alias Model =
    { error : Maybe String
    , runners : List Runner
    , query : String
    , searchTerm : Maybe String
    , active : Bool
    , zone : Time.Zone
    }


type alias Runner =
    { id : String
    , name : String
    , location : String
    , age : Int
    , bib : Int
    , estimatedDistance : Float
    , lastMarkerDistance : Float
    , lastMarkerTime : Float
    , pace : Float
    }


type alias RunnerWsMsg =
    { name : String
    , runner : Runner
    }


initModel : Model
initModel =
    { error = Nothing
    , runners = []
    , query = ""
    , searchTerm = Nothing
    , active = False
    , zone = Time.utc
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.batch
        [ listenRunnersMsg (encodeMsg "listen runners" JE.null)
        , Task.perform AdjustTimeZone Time.here
        ]
    )


encodeMsg : String -> JE.Value -> String
encodeMsg name data =
    JE.object
        [ ( "name", JE.string name )
        , ( "data", data )
        ]
        |> JE.encode 0



-- update


type Msg
    = SearchInput String
    | Search
    | WsMessage String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        Search ->
            let
                searchTerm =
                    if String.isEmpty model.query then
                        Nothing

                    else
                        Just model.query
            in
            ( { model | searchTerm = searchTerm }, Cmd.none )

        WsMessage wsMsg ->
            wsMessage wsMsg model

        Tick time ->
            ( tick model time, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


tick : Model -> Time.Posix -> Model
tick model time =
    let
        updatedRunners =
            List.map (advanceDistance time)
                model.runners
    in
    { model | runners = updatedRunners }


advanceDistance : Time.Posix -> Runner -> Runner
advanceDistance time runner =
    let
        elapsedMinutes =
            (toFloat (Time.posixToMillis time) - runner.lastMarkerTime) / 1000 / 60
    in
    if runner.lastMarkerTime > 0 then
        { runner | estimatedDistance = runner.lastMarkerDistance + (runner.pace * elapsedMinutes) }

    else
        runner


wsMessage : String -> Model -> ( Model, Cmd Msg )
wsMessage wsMsg model =
    case JD.decodeString msgDecoder wsMsg of
        Ok { name, runner } ->
            case name of
                "new runner" ->
                    ( { model | runners = runner :: model.runners }
                    , Cmd.none
                    )

                "update runner" ->
                    let
                        updatedRunners =
                            List.map
                                (\r ->
                                    if r.id == runner.id then
                                        runner

                                    else
                                        r
                                )
                                model.runners
                    in
                    ( { model
                        | runners = updatedRunners
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | error = Just ("Unrecognized Message: " ++ name) }
                    , Cmd.none
                    )

        Err err ->
            ( { model | error = Just (JD.errorToString err) }, Cmd.none )


msgDecoder : JD.Decoder RunnerWsMsg
msgDecoder =
    JD.succeed RunnerWsMsg
        |> JDP.required "name" JD.string
        |> JDP.required "data" runnerDecoder


runnerDecoder : JD.Decoder Runner
runnerDecoder =
    JD.succeed Runner
        |> JDP.required "_id" JD.string
        |> JDP.required "name" JD.string
        |> JDP.required "location" JD.string
        |> JDP.required "age" JD.int
        |> JDP.required "bib" JD.int
        |> JDP.hardcoded 0
        |> JDP.required "lastMarkerDistance" JD.float
        |> JDP.required "lastMarkerTime" JD.float
        |> JDP.required "pace" JD.float



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , searchForm model.query
        , List.filter
            (\runner ->
                model.searchTerm
                    |> Maybe.map
                        (\term ->
                            String.contains term runner.name
                        )
                    |> Maybe.withDefault True
            )
            model.runners
            |> List.map
                (\runner ->
                    let
                        { name, location, age, bib, estimatedDistance } =
                            runner
                    in
                    tr []
                        [ td [] [ text name ]
                        , td [] [ text location ]
                        , td [] [ text (toString age) ]
                        , td [] [ text (toString bib) ]
                        , td [] [ lastMarker runner model.zone ]
                        , td [] [ text (formatDistance estimatedDistance) ]
                        ]
                )
            |> tbody []
            |> (\t -> [ runnersHeader, t ])
            |> table []
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


lastMarker : Runner -> Time.Zone -> Html Msg
lastMarker runner zone =
    if runner.lastMarkerTime > 0 then
        let
            hour =
                runner.lastMarkerTime
                    |> round
                    |> Time.millisToPosix
                    |> Time.toHour zone
                    |> String.fromInt

            minute =
                runner.lastMarkerTime
                    |> round
                    |> Time.millisToPosix
                    |> Time.toMinute zone
                    |> String.fromInt

            second =
                runner.lastMarkerTime
                    |> round
                    |> Time.millisToPosix
                    |> Time.toSecond zone
                    |> String.fromInt
        in
        text
            (formatDistance runner.lastMarkerDistance
                ++ " mi @ "
                ++ addZero hour
                ++ ":"
                ++ addZero minute
                ++ ":"
                ++ addZero second
            )

    else
        text ""


addZero : String -> String
addZero minute =
    if Maybe.withDefault 0 (String.toInt minute) < 10 then
        "0" ++ minute

    else
        minute


formatDistance : Float -> String
formatDistance distance =
    if distance <= 0 then
        ""

    else
        distance
            * 100
            |> round
            |> toFloat
            |> flip (/) 100
            |> toString


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
    Sub.batch
        [ listen WsMessage
        , Time.every 1000 Tick
        ]
