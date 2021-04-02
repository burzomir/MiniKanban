module TodoApp exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode
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


getEntry : ID -> EntriesCollection -> Maybe Entry
getEntry =
    Dict.get


type alias Model =
    { entries : Dict ID Entry
    , error : String
    }


type Msg
    = Initialized (List Entry)
    | AddEntry
    | EntryAdded Entry
    | EntryTitleChanged ID Title
    | EntryStatusChanged ID Status
    | EntryDeleted ID
    | ErrorOccured String
    | NothingHappenned


init : () -> ( Model, Cmd Msg )
init _ =
    ( { entries = Dict.empty, error = "" }
    , getAllEntries
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialized entries ->
            ( { model | entries = map (\entry -> ( entry.id, entry )) entries |> Dict.fromList }, Cmd.none )

        AddEntry ->
            ( { model | error = "" }, createEntry )

        EntryAdded entry ->
            ( { model | entries = insertEntry entry model.entries, error = "" }, Cmd.none )

        EntryTitleChanged id title ->
            let
                entries =
                    changeEntryTitle id title model.entries

                cmd =
                    getEntry id entries |> Maybe.map updateEntry |> Maybe.withDefault Cmd.none
            in
            ( { model | entries = entries, error = "" }, cmd )

        EntryStatusChanged id status ->
            let
                entries =
                    changeEntryStatus id status model.entries

                cmd =
                    getEntry id entries |> Maybe.map updateEntry |> Maybe.withDefault Cmd.none
            in
            ( { model | entries = entries, error = "" }, cmd )

        EntryDeleted id ->
            ( { model | entries = deleteEntry id model.entries, error = "" }, Cmd.none )

        ErrorOccured error ->
            ( { model | error = error }, Cmd.none )

        NothingHappenned ->
            ( model, Cmd.none )


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


createEntry : Cmd Msg
createEntry =
    Http.post
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos"
        , body = Http.emptyBody
        , expect = Http.expectJson processEntryCreationResult entryDecoder
        }


processEntryCreationResult : Result Http.Error Entry -> Msg
processEntryCreationResult res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    ErrorOccured d

                _ ->
                    ErrorOccured "Some error occured"

        Ok entry ->
            EntryAdded entry


getAllEntries : Cmd Msg
getAllEntries =
    Http.get
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos"
        , expect = Http.expectJson processAllEntriesResult entriesDecoder
        }


processAllEntriesResult : Result Http.Error (List Entry) -> Msg
processAllEntriesResult res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    ErrorOccured d

                _ ->
                    ErrorOccured "Some error occured"

        Ok entries ->
            Initialized entries


updateEntry : Entry -> Cmd Msg
updateEntry entry =
    Http.request
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos/" ++ entry.id
        , method = "PUT"
        , headers = []
        , body = Http.jsonBody <| entryEncoder entry
        , expect = Http.expectJson processUpdateEntryResult entryDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


processUpdateEntryResult : Result Http.Error Entry -> Msg
processUpdateEntryResult res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    ErrorOccured d

                _ ->
                    ErrorOccured "Some error occured"

        Ok _ ->
            NothingHappenned


entriesDecoder : Json.Decode.Decoder (List Entry)
entriesDecoder =
    Json.Decode.list entryDecoder


entryDecoder : Json.Decode.Decoder Entry
entryDecoder =
    Json.Decode.map3 Entry idDecoder titleDecoder statusDecoder


entryEncoder : Entry -> Json.Encode.Value
entryEncoder entry =
    Json.Encode.object
        [ ( "title", Json.Encode.string entry.title )
        , ( "status", statusEncoder entry.status )
        ]


statusEncoder : Status -> Json.Encode.Value
statusEncoder status =
    let
        s =
            case status of
                Todo ->
                    "Todo"

                InProgress ->
                    "InProgress"

                Done ->
                    "Done"
    in
    Json.Encode.string s


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
