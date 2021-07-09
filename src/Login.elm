module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- model


type alias Model =
    { username : String
    , password : String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    }



-- update


type Msg
    = UsernameInput String
    | PasswordInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameInput username ->
            Debug.log "Input updated model"
                { model | username = username }

        PasswordInput password ->
            Debug.log "Input updated model"
                { model | password = password }



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Login Page.." ]
        , Html.form []
            [ input
                [ type_ "text"
                , onInput UsernameInput
                , placeholder "username"
                ]
                []
            , input
                [ type_ "password"
                , onInput PasswordInput
                , placeholder "password"
                ]
                []
            , input
                [ type_ "submit" ]
                [ text "Login" ]
            ]
        ]
