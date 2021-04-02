module App exposing (EntriesRepo, run)

import Browser
import Dict
import EntriesCollection exposing (EntriesCollection)
import Entry exposing (Entry, ID, Status(..), Title)
import Html exposing (Html, button, div, h3, input, text)
import Html.Attributes exposing (class, draggable, style, title, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode
import List exposing (map)
import Maybe exposing (Maybe(..))


run : EntriesRepo String Msg -> Program () Model Msg
run repo =
    Browser.element { init = init repo, subscriptions = subscriptions, view = view, update = update repo }


type alias Model =
    { entries : EntriesCollection.EntriesCollection
    , error : String
    , liftedEntry : Maybe ID
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
    | EntryLifted ID
    | EntryDropped Status


init : EntriesRepo String Msg -> () -> ( Model, Cmd Msg )
init repo _ =
    ( { entries = Dict.empty, error = "", liftedEntry = Nothing }
    , repo.getAll ErrorOccured Initialized
    )


update : EntriesRepo String Msg -> Msg -> Model -> ( Model, Cmd Msg )
update repo msg model =
    case msg of
        Initialized entries ->
            ( { model | entries = entries }, Cmd.none )

        AddEntry ->
            ( { model | error = "" }, repo.create ErrorOccured EntryAdded )

        EntryAdded entry ->
            ( { model | entries = EntriesCollection.insertEntry entry model.entries, error = "" }, Cmd.none )

        EntryTitleChanged id title ->
            let
                entries =
                    EntriesCollection.changeEntryTitle id title model.entries

                cmd =
                    EntriesCollection.getEntry id entries
                        |> Maybe.map (repo.update ErrorOccured (\_ -> NothingHappenned))
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | entries = entries, error = "" }, cmd )

        EntryStatusChanged id status ->
            let
                entries =
                    EntriesCollection.changeEntryStatus id status model.entries

                cmd =
                    EntriesCollection.getEntry id entries
                        |> Maybe.map (repo.update ErrorOccured (\_ -> NothingHappenned))
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | entries = entries, error = "" }, cmd )

        EntryDeleted id ->
            ( { model | entries = EntriesCollection.deleteEntry id model.entries, error = "" }
            , repo.delete ErrorOccured (\_ -> NothingHappenned) id
            )

        ErrorOccured error ->
            ( { model | error = error }, Cmd.none )

        NothingHappenned ->
            ( model, Cmd.none )

        EntryLifted id ->
            ( { model | liftedEntry = Just id }, Cmd.none )

        EntryDropped status ->
            case model.liftedEntry of
                Nothing ->
                    ( { model | liftedEntry = Nothing }, Cmd.none )

                Just id ->
                    let
                        entries =
                            EntriesCollection.changeEntryStatus id status model.entries

                        cmd =
                            EntriesCollection.getEntry id entries
                                |> Maybe.map (repo.update ErrorOccured (\_ -> NothingHappenned))
                                |> Maybe.withDefault Cmd.none
                    in
                    ( { model | entries = entries, error = "" }, cmd )


view : Model -> Html Msg
view model =
    let
        entries =
            Dict.values model.entries |> List.sortBy .title

        buttons =
            [ div [class "p-1"] [ button [ class "ring rounded-md font-semibold text-white bg-blue-500 ring p-1", onClick AddEntry ] [ text "Add" ] ] ]

        errors =
            [ div [ style "color" "red" ] [ text model.error ] ]
    in
    div []
        [ div [ style "display" "flex" ]
            [ viewLane entries Todo "Todo"
            , viewLane entries InProgress "In progress"
            , viewLane entries Done "Done"
            ]
        , div [] <| buttons ++ errors
        ]


viewLane : List Entry -> Status -> String -> Html Msg
viewLane entries status header =
    div
        [ preventDefaultOn "dragover" (Json.Decode.succeed ( NothingHappenned, True ))
        , preventDefaultOn "drop" (Json.Decode.succeed ( EntryDropped status, True ))
        ]
        (h3 [class "text-xl p-1"] [ text header ] :: (map viewEntry <| List.filter (\e -> e.status == status) entries))


viewEntry : Entry -> Html Msg
viewEntry entry =
    div
        [ draggable "true"
        , on "dragstart" (Json.Decode.succeed <| EntryLifted entry.id)
        , class "border rounded border-gray-300 m-1 p-1"
        ]
        [ input [ value entry.title, onInput (EntryTitleChanged entry.id) ] []
        , button [ onClick (EntryDeleted entry.id) ] [ text "🗑️" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias EntriesRepo err msg =
    { create : (err -> msg) -> (Entry -> msg) -> Cmd msg
    , update : (err -> msg) -> (Entry -> msg) -> Entry -> Cmd msg
    , delete : (err -> msg) -> (Entry -> msg) -> ID -> Cmd msg
    , getAll : (err -> msg) -> (EntriesCollection -> msg) -> Cmd msg
    }
