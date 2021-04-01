module Forms exposing (..)

import Browser
import Html exposing (Html, div, form, input, label, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { username : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    { username = "", password = "", passwordAgain = "" }


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | PasswordAgainChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameChanged v ->
            { model | username = v }

        PasswordChanged v ->
            { model | password = v }

        PasswordAgainChanged v ->
            { model | passwordAgain = v }


view : Model -> Html Msg
view model =
    form []
        [ viewTextInput "Username" model.username UsernameChanged
        , viewPasswordInput "Password" model.password PasswordChanged
        , viewPasswordInput "Password again" model.passwordAgain PasswordAgainChanged
        , viewValidation model.password model.passwordAgain
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t l v m =
    div []
        [ label [] [ text l, input [ value v, onInput m, type_ t ] [] ] ]


viewTextInput : String -> String -> (String -> msg) -> Html msg
viewTextInput =
    viewInput "text"


viewPasswordInput : String -> String -> (String -> msg) -> Html msg
viewPasswordInput =
    viewInput "password"


viewValidation : String -> String -> Html msg
viewValidation p pa =
    let
        content =
            if p == pa then
                []

            else
                [ text "Passwords do not match" ]
    in
    div [ style "color" "red" ] content
