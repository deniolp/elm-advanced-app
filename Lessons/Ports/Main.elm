port module Ports.Main exposing (..)

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


type alias Customer =
    { id : Int
    , name : String
    }


type alias Model =
    { name : String
    , customers : List Customer
    , error : Maybe String
    , nextId : Int
    }


initModel : Model
initModel =
    { name = ""
    , customers = []
    , error = Nothing
    , nextId = 1
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- update


type Msg
    = NameInput String
    | SaveCustomer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model | name = name }, Cmd.none )

        SaveCustomer ->
            -- let
            --     newCustomer =
            --         Customer model.nextId model.name
            --     newCustomers =
            --         newCustomer :: model.customers
            --     newModel =
            --         { model
            --             | customers = newCustomers
            --             , nextId = model.nextId + 1
            --             , name = ""
            --         }
            -- in
            -- ( newModel, Cmd.none )
            ( model, addCustomer model.name )



--view


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Customer List" ]
        , viewCustomerForm model
        , viewCustomers model.customers
        ]


viewCustomerForm : Model -> Html Msg
viewCustomerForm model =
    Html.form [ onSubmit SaveCustomer ]
        [ input [ type_ "text", onInput NameInput, value model.name ] []
        , text <| Maybe.withDefault "" model.error
        , button [ type_ "submit" ] [ text "Save" ]
        ]


viewCustomers : List Customer -> Html Msg
viewCustomers customers =
    customers
        |> List.sortBy .id
        |> List.map viewCustomer
        |> ul []


viewCustomer : Customer -> Html Msg
viewCustomer customer =
    li []
        [ i [ class "remove" ] []
        , text customer.name
        ]



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--ports


port addCustomer : String -> Cmd msg
