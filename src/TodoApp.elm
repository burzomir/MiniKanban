module TodoApp exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import List exposing (map)
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
    String


type alias Entry =
    { id : ID, title : Title, status : Status }


type alias EntriesCollection =
    Dict ID Entry


insertEntry : Entry -> EntriesCollection -> EntriesCollection
insertEntry entry =
    Dict.insert entry.id entry


changeEntryTitle : ID -> Title -> EntriesCollection -> EntriesCollection
changeEntryTitle id title =
    Dict.update id (Maybe.map (\v -> { v | title = title }))


changeEntryStatus : ID -> Status -> EntriesCollection -> EntriesCollection
changeEntryStatus id status =
    Dict.update id (Maybe.map (\v -> { v | status = status }))


deleteEntry : ID -> EntriesCollection -> EntriesCollection
deleteEntry =
    Dict.remove


type alias Model =
    { entries : Dict ID Entry
    , error : String
    }


type Msg
    = AddEntry
    | EntryAdded Entry
    | EntryTitleChanged ID Title
    | EntryStatusChanged ID Status
    | EntryDeleted ID
    | ErrorOccured String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { entries = Dict.empty, error = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddEntry ->
            ( { model | error = "" }, createEntry )

        EntryAdded entry ->
            ( { model | entries = insertEntry entry model.entries, error = "" }, Cmd.none )

        EntryTitleChanged id title ->
            ( { model | entries = changeEntryTitle id title model.entries, error = "" }, Cmd.none )

        EntryStatusChanged id status ->
            ( { model | entries = changeEntryStatus id status model.entries, error = "" }, Cmd.none )

        EntryDeleted id ->
            ( { model | entries = deleteEntry id model.entries, error = "" }, Cmd.none )

        ErrorOccured error ->
            ( { model | error = error }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        entries =
            Dict.values model.entries |> map viewEntry

        buttons =
            [ div [] [ button [ onClick AddEntry ] [ text "Add" ] ] ]

        errors =
            [ div [ style "color" "red" ] [ text model.error ] ]
    in
    div [] (entries ++ buttons ++ errors)


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
        [ text entry.id
        , input [ value entry.title, onInput (EntryTitleChanged entry.id) ] []
        , actionButton
        , button [ onClick (EntryDeleted entry.id) ] [ text "Delete" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


createEntry =
    Http.post
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos"
        , body = Http.emptyBody
        , expect = Http.expectJson processResult entryDecoder
        }


processResult : Result Http.Error Entry -> Msg
processResult res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    ErrorOccured d

                _ ->
                    ErrorOccured "Some error occured"

        Ok entry ->
            EntryAdded entry


entryDecoder : Json.Decode.Decoder Entry
entryDecoder =
    Json.Decode.map3 Entry idDecoder titleDecoder statusDecoder


idDecoder : Json.Decode.Decoder ID
idDecoder =
    Json.Decode.field "id" Json.Decode.string


titleDecoder : Json.Decode.Decoder Title
titleDecoder =
    Json.Decode.field "title" Json.Decode.string


statusDecoder : Json.Decode.Decoder Status
statusDecoder =
    let
        decoder =
            Json.Decode.string
                |> Json.Decode.andThen
                    (\s ->
                        case s of
                            "Todo" ->
                                Json.Decode.succeed Todo

                            "InProgress" ->
                                Json.Decode.succeed InProgress

                            "Done" ->
                                Json.Decode.succeed Done

                            somethingElse ->
                                Json.Decode.fail <| "Unknown status: " ++ somethingElse
                    )
    in
    Json.Decode.field "status" decoder