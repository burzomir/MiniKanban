module App exposing (EntriesRepo, run)

import Browser
import Dict exposing (Dict)
import EntriesCollection exposing (EntriesCollection)
import Entry exposing (Entry, ID, Status(..), Title)
import Html exposing (Html, button, div, h3, input, text)
import Html.Attributes exposing (class, draggable, style, title, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn, stopPropagationOn)
import Json.Decode
import Lane
import List exposing (map)
import Maybe exposing (Maybe(..))
import Maybe.Extra exposing (isJust)


run : EntriesRepo String Msg -> Program () Model Msg
run repo =
    Browser.element { init = init repo, subscriptions = subscriptions, view = view, update = update repo }


type alias Model =
    { entries : EntriesCollection.EntriesCollection
    , lanes : Dict Lane.ID Lane.Lane
    , error : String
    , liftedEntry : Maybe ID
    , enteredLane : Maybe Lane.ID
    , enteredEntry : Maybe Int
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
    | EntryDropped Lane.ID
    | LaneEntered Lane.ID
    | LaneLeft Lane.ID
    | EntryEntered Int
    | EntryLeft


init : EntriesRepo String Msg -> () -> ( Model, Cmd Msg )
init repo _ =
    ( initModel
    , repo.getAll ErrorOccured Initialized
    )


initModel : Model
initModel =
    { entries = Dict.empty
    , lanes = initLanes
    , error = ""
    , liftedEntry = Nothing
    , enteredLane = Nothing
    , enteredEntry = Nothing
    }


initLanes : Dict ID Lane.Lane
initLanes =
    Dict.fromList
        [ ( "1", { id = "1", title = "To do", entries = [ "1", "17", "18", "19" ] } )
        , ( "2", { id = "2", title = "In progress", entries = [ "2" ] } )
        , ( "3", { id = "3", title = "Done", entries = [ "20" ] } )
        ]


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

        EntryDropped laneId ->
            case model.liftedEntry of
                Nothing ->
                    ( { model | liftedEntry = Nothing }, Cmd.none )

                Just entryId ->
                    let
                        index =
                            model.enteredEntry |> Maybe.withDefault 0

                        lanes =
                            Dict.map
                                (\_ lane ->
                                    let
                                        l =
                                            Lane.remove entryId lane
                                    in
                                    if l.id == laneId then
                                        Lane.insert index entryId l

                                    else
                                        l
                                )
                                model.lanes
                    in
                    ( { model | lanes = lanes, error = "", liftedEntry = Nothing, enteredLane = Nothing, enteredEntry = Nothing }, Cmd.none )

        LaneEntered laneId ->
            ( { model | enteredLane = Just laneId }, Cmd.none )

        LaneLeft laneId ->
            ( { model
                | enteredLane =
                    model.enteredLane
                        |> Maybe.andThen
                            (\id ->
                                if id == laneId then
                                    Nothing

                                else
                                    Just id
                            )
              }
            , Cmd.none
            )

        EntryEntered index ->
            ( { model | enteredEntry = Just index }, Cmd.none )

        EntryLeft ->
            ( { model | enteredEntry = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        buttons =
            [ div [ class "p-1" ] [ button [ class "ring rounded-md font-semibold text-white bg-blue-500 ring p-1", onClick AddEntry ] [ text "Add" ] ] ]

        errors =
            [ div [ style "color" "red" ] [ text model.error ] ]
    in
    div []
        [ div [ class "flex" ] (Dict.values model.lanes |> map (viewLane model.enteredEntry model.entries))
        , div [] <| buttons ++ errors
        ]


viewLane : Maybe Int -> EntriesCollection -> Lane.Lane -> Html Msg
viewLane liftedEntryIndex entries lane =
    let
        viewEntries =
            lane.entries
                |> map (\id -> EntriesCollection.getEntry id entries)
                |> List.foldr (\e es -> e |> Maybe.map List.singleton |> Maybe.withDefault [] |> (\e_ -> e_ ++ es)) []
                |> List.indexedMap (viewEntry liftedEntryIndex)

        dragLeave =
            if isJust liftedEntryIndex then
                []

            else
                [ on "dragleave" (Json.Decode.succeed (LaneLeft lane.id)) ]
    in
    div
        ([ preventDefaultOn "dragover" (Json.Decode.succeed ( NothingHappenned, True ))
         , preventDefaultOn "drop" (Json.Decode.succeed ( EntryDropped lane.id, True ))
         , on "dragenter" (Json.Decode.succeed (LaneEntered lane.id))
         , class "flex-1"
         ]
            ++ dragLeave
        )
        (h3 [ class "text-xl p-1" ] [ text lane.title ] :: viewEntries)


viewEntry : Maybe Int -> Int -> Entry -> Html Msg
viewEntry liftedEntryIndex index entry =
    let
        dropIndicator =
            if liftedEntryIndex == Just index then
                [ style "border-top" "4px solid blue" ]

            else
                []
    in
    div
        ([ draggable "true"
         , on "dragstart" (Json.Decode.succeed <| EntryLifted entry.id)
         , stopPropagationOn "dragenter" (Json.Decode.succeed ( EntryEntered index, True ))
         , stopPropagationOn "dragleave" (Json.Decode.succeed ( EntryLeft, True ))
         , class "border rounded border-gray-300 m-1 p-1 flex"
         ]
            ++ dropIndicator
        )
        [ input [ style "pointer-events" "none", value entry.title, onInput (EntryTitleChanged entry.id), class "flex-grow" ] []
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
