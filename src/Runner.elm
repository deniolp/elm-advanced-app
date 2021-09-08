module Runner exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD exposing (field)
import Json.Encode as JE
import Utils exposing (ErrorDetailed, expectJsonCustom)



-- model


type alias Model =
    { id : String
    , name : String
    , nameError : Maybe String
    , location : String
    , locationError : Maybe String
    , age : String
    , ageError : Maybe String
    , bib : String
    , bibError : Maybe String
    , error : Maybe String
    }


initModel : Model
initModel =
    { id = ""
    , name = ""
    , nameError = Nothing
    , location = ""
    , locationError = Nothing
    , age = ""
    , ageError = Nothing
    , bib = ""
    , bibError = Nothing
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = NameInput String
    | LocationInput String
    | AgeInput String
    | BibInput String
    | Save
    | SaveResponse (Result (ErrorDetailed String) ( Http.Metadata, String ))


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update token msg model =
    case msg of
        NameInput name ->
            ( { model
                | name = name
                , nameError = Nothing
              }
            , Cmd.none
            )

        LocationInput location ->
            ( { model
                | location = location
                , locationError = Nothing
              }
            , Cmd.none
            )

        AgeInput age ->
            ageInput age model

        BibInput bib ->
            bibInput bib model

        Save ->
            let
                updatedModel =
                    validate model
            in
            if isValid updatedModel then
                save token updatedModel

            else
                ( updatedModel, Cmd.none )

        SaveResponse (Ok _) ->
            ( initModel, Cmd.none )

        SaveResponse (Err err) ->
            let
                errMsg =
                    case err of
                        Utils.BadStatus meta body ->
                            case meta.statusCode of
                                400 ->
                                    body

                                _ ->
                                    meta.statusText

                        Utils.BadBody _ _ error ->
                            error

                        _ ->
                            "Error Saving!"
            in
            ( { model | error = Just errMsg }, Cmd.none )


runnerEncoder : Model -> JE.Value
runnerEncoder { name, location, age, bib } =
    let
        ageInt =
            age |> String.toInt |> Maybe.withDefault 0

        bibInt =
            bib |> String.toInt |> Maybe.withDefault 0
    in
    JE.object
        [ ( "name", JE.string name )
        , ( "location", JE.string location )
        , ( "age", JE.int ageInt )
        , ( "bib", JE.int bibInt )
        ]


save : String -> Model -> ( Model, Cmd Msg )
save token model =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ token) ]

        body =
            Http.jsonBody <| runnerEncoder model

        decoder =
            field "_id" JD.string

        cmd =
            post "http://localhost:5000/runner" headers body decoder
    in
    ( model, cmd )


post : String -> List Http.Header -> Http.Body -> JD.Decoder String -> Cmd Msg
post url headers body decoder =
    Http.request
        { method = "POST"
        , headers = headers
        , url = url
        , body = body
        , expect = expectJsonCustom SaveResponse decoder
        , timeout = Nothing
        , tracker = Nothing
        }


isValid : Model -> Bool
isValid model =
    model.nameError
        == Nothing
        && model.locationError
        == Nothing
        && model.ageError
        == Nothing
        && model.bibError
        == Nothing


validate : Model -> Model
validate model =
    model
        |> validateName
        |> validateLocation
        |> validateAge
        |> validateBib


validateName : Model -> Model
validateName model =
    if String.isEmpty model.name then
        { model
            | nameError = Just "Name is required"
        }

    else
        { model
            | nameError = Nothing
        }


validateLocation : Model -> Model
validateLocation model =
    if String.isEmpty model.location then
        { model
            | locationError = Just "Name is required"
        }

    else
        { model
            | locationError = Nothing
        }


validateAge : Model -> Model
validateAge model =
    let
        ageInt =
            model.age
                |> String.toInt
                |> Maybe.withDefault 0
    in
    if ageInt <= 0 then
        { model
            | ageError = Just "Age must be a positive number"
        }

    else
        { model
            | ageError = Nothing
        }


validateBib : Model -> Model
validateBib model =
    let
        bibInt =
            model.bib
                |> String.toInt
                |> Maybe.withDefault 0
    in
    if bibInt <= 0 then
        { model
            | bibError = Just "Bib must be a positive number"
        }

    else
        { model
            | bibError = Nothing
        }


ageInput : String -> Model -> ( Model, Cmd Msg )
ageInput age model =
    let
        ageInt =
            age
                |> String.toInt
                |> Maybe.withDefault 0

        ageError =
            if ageInt <= 0 then
                Just "Must enter a positive number"

            else
                Nothing
    in
    ( { model | age = age, ageError = ageError }, Cmd.none )


bibInput : String -> Model -> ( Model, Cmd Msg )
bibInput bib model =
    let
        bibInt =
            bib
                |> String.toInt
                |> Maybe.withDefault 0

        bibError =
            if bibInt <= 0 then
                Just "Must enter a positive number"

            else
                Nothing
    in
    ( { model | bib = bib, bibError = bibError }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , viewForm model
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ class "add-runner", onSubmit Save ]
        [ fieldset []
            [ legend [] [ text "Add / Edit Runner" ]
            , div []
                [ label [] [ text "Name" ]
                , input
                    [ type_ "text"
                    , value model.name
                    , onInput NameInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.nameError ]
                ]
            , div []
                [ label [] [ text "Location" ]
                , input
                    [ type_ "text"
                    , value model.location
                    , onInput LocationInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.locationError ]
                ]
            , div []
                [ label [] [ text "Age" ]
                , input
                    [ type_ "text"
                    , value model.age
                    , onInput AgeInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.ageError ]
                ]
            , div []
                [ label [] [ text "Bib #" ]
                , input
                    [ type_ "text"
                    , value model.bib
                    , onInput BibInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.bibError ]
                ]
            , div []
                [ label [] []
                , button [ type_ "submit" ] [ text "Save" ]
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
