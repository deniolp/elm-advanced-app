module LeaderBoard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- model


type alias Model =
    { runners : List Runner
    , query : String
    }


type alias Runner =
    { id : Int
    , name : String
    , location : String
    }


initModel : Model
initModel =
    { runners = []
    , query = ""
    }



-- update


type Msg
    = QueryInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        QueryInput query ->
            Debug.log "Input query updated model"
                { model | query = query }



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Leaderboard page.." ]
        , input
            [ type_ "text"
            , onInput QueryInput
            , value model.query
            , placeholder "Search for a runner..."
            ]
            []
        ]
