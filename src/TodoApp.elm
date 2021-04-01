module TodoApp exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import List exposing (map, maximum)
import Maybe exposing (Maybe(..))


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, view = view, update = update }


type Status
    = Todo
    | InProgress
    | Done


type alias Title =
    String


type alias ID =
    Int


type alias Entry =
    { id : ID, title : Title, status : Status }


type alias Model =
    Dict ID Entry


type Msg
    = EntryAdded
    | EntryTitleChanged ID Title
    | EntryStatusChanged ID Status
    | EntryDeleted ID


init : () -> ( Model, Cmd Msg )
init _ =
    ( Dict.empty, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EntryAdded ->
            let
                id =
                    Dict.values model |> map .id |> maximum |> Maybe.withDefault 0 |> (\v -> v + 1)
            in
            ( Dict.insert id (Entry id "" Todo) model, Cmd.none )

        EntryTitleChanged id title ->
            ( Dict.update id (Maybe.map (\v -> { v | title = title })) model, Cmd.none )

        EntryStatusChanged id status ->
            ( Dict.update id (Maybe.map (\v -> { v | status = status })) model, Cmd.none )

        EntryDeleted id ->
            ( Dict.remove id model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        entries =
            Dict.values model |> map viewEntry
    in
    div [] (List.append entries [ div [] [ button [ onClick EntryAdded ] [ text "Add" ] ] ])


viewEntry : Entry -> Html Msg
viewEntry entry =
    let
        actionButton =
            case entry.status of
                Todo ->
                    button [ onClick (EntryStatusChanged entry.id InProgress) ] [ text "Start progress" ]

                InProgress ->
                    button [ onClick (EntryStatusChanged entry.id Done) ] [ text "Finish" ]

                Done ->
                    text ""
    in
    div []
        [ text (String.fromInt entry.id)
        , input [ value entry.title, onInput (EntryTitleChanged entry.id) ] []
        , actionButton
        , button [ onClick (EntryDeleted entry.id) ] [ text "Delete" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
