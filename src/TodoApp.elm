module TodoApp exposing (..)

import Browser
import Dict exposing (Dict)
import EntriesCollection exposing (EntriesCollection)
import Entry exposing (Entry, ID, Status(..), Title)
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


type alias Model =
    { entries : EntriesCollection.EntriesCollection
    , error : String
    }


type Msg
    = Initialized EntriesCollection.EntriesCollection
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
            ( { model | entries = entries }, Cmd.none )

        AddEntry ->
            ( { model | error = "" }, createEntry )

        EntryAdded entry ->
            ( { model | entries = EntriesCollection.insertEntry entry model.entries, error = "" }, Cmd.none )

        EntryTitleChanged id title ->
            let
                entries =
                    EntriesCollection.changeEntryTitle id title model.entries

                cmd =
                    EntriesCollection.getEntry id entries |> Maybe.map updateEntry |> Maybe.withDefault Cmd.none
            in
            ( { model | entries = entries, error = "" }, cmd )

        EntryStatusChanged id status ->
            let
                entries =
                    EntriesCollection.changeEntryStatus id status model.entries

                cmd =
                    EntriesCollection.getEntry id entries |> Maybe.map updateEntry |> Maybe.withDefault Cmd.none
            in
            ( { model | entries = entries, error = "" }, cmd )

        EntryDeleted id ->
            ( { model | entries = EntriesCollection.deleteEntry id model.entries, error = "" }, deleteEntryFromApi id )

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
        , expect = Http.expectJson processEntryCreationResult Entry.decode
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
        , expect = Http.expectJson processAllEntriesResult EntriesCollection.decode
        }


processAllEntriesResult : Result Http.Error EntriesCollection.EntriesCollection -> Msg
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
        , body = Http.jsonBody <| Entry.encode entry
        , expect = Http.expectJson processUpdateEntryResult Entry.decode
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


deleteEntryFromApi : ID -> Cmd Msg
deleteEntryFromApi id =
    Http.request
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos/" ++ id
        , method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson processUpdateEntryResult Entry.decode
        , timeout = Nothing
        , tracker = Nothing
        }


processDeleteEntryResult : Result Http.Error Entry -> Msg
processDeleteEntryResult res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    ErrorOccured d

                _ ->
                    ErrorOccured "Some error occured"

        Ok _ ->
            NothingHappenned