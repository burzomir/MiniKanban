module App exposing (EntriesRepo, LanesRepo, Model, Msg, init, subscriptions, update, view)

import Dict
import DragDrop as DD
import EntriesCollection exposing (EntriesCollection)
import Entry exposing (Entry, ID, Title)
import Html exposing (Html, button, div, h1, h3, input, text)
import Html.Attributes exposing (class, draggable, style, title, value)
import Html.Events exposing (onClick, onInput)
import Lane
import LanesCollection
import List exposing (map)
import Maybe exposing (Maybe(..))
import Maybe.Extra exposing (isJust)


type alias Model =
    { entries : EntriesCollection.EntriesCollection
    , lanes : LanesCollection.LanesCollection
    , error : String
    , dragDrop : DD.Model
    }


type Msg
    = EntriesInitialized EntriesCollection.EntriesCollection
    | AddEntry
    | EntryAdded Entry
    | EntryTitleChanged ID Title
    | EntryDeleted ID
    | LanesInitialized LanesCollection.LanesCollection
    | AddLane
    | LaneAdded Lane.Lane
    | LaneTitleChanged Lane.ID Lane.Title
    | LaneDeleted Lane.ID
    | ErrorOccured String
    | NothingHappenned
    | DragDropMsg DD.Msg
    | EntryDropped DD.Msg


type alias Env =
    { entriesRepo : EntriesRepo String Msg
    , lanesRepo : LanesRepo String Msg
    }


init : Env -> () -> ( Model, Cmd Msg )
init env _ =
    ( initModel
    , Cmd.batch
        [ env.entriesRepo.getAll ErrorOccured EntriesInitialized
        , env.lanesRepo.getAll ErrorOccured LanesInitialized
        ]
    )


initModel : Model
initModel =
    { entries = EntriesCollection.empty
    , lanes = LanesCollection.empty
    , error = ""
    , dragDrop = DD.init
    }


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update env msg model =
    case msg of
        EntriesInitialized entries ->
            ( { model | entries = entries }, Cmd.none )

        AddEntry ->
            ( { model | error = "" }, env.entriesRepo.create ErrorOccured EntryAdded )

        EntryAdded entry ->
            ( { model | entries = EntriesCollection.insertEntry entry model.entries, error = "" }, Cmd.none )

        EntryTitleChanged id title ->
            let
                entries =
                    EntriesCollection.changeEntryTitle id title model.entries

                cmd =
                    EntriesCollection.getEntry id entries
                        |> Maybe.map (env.entriesRepo.update ErrorOccured (\_ -> NothingHappenned))
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | entries = entries, error = "" }, cmd )

        EntryDeleted id ->
            ( { model | entries = EntriesCollection.deleteEntry id model.entries, error = "" }
            , env.entriesRepo.delete ErrorOccured (\_ -> NothingHappenned) id
            )

        LanesInitialized lanesCollection ->
            ( { model | lanes = lanesCollection }, Cmd.none )

        AddLane ->
            ( model, env.lanesRepo.create ErrorOccured LaneAdded )

        LaneAdded lane ->
            ( { model | lanes = LanesCollection.insert lane model.lanes, error = "" }, Cmd.none )

        LaneTitleChanged laneId title ->
            let
                lanes =
                    LanesCollection.changeTitle laneId title model.lanes

                cmd =
                    LanesCollection.get laneId lanes
                        |> Maybe.map (env.lanesRepo.update ErrorOccured (\_ -> NothingHappenned))
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | lanes = lanes, error = "" }, cmd )

        LaneDeleted laneId ->
            ( { model | lanes = LanesCollection.delete laneId model.lanes, error = "" }
            , env.lanesRepo.delete ErrorOccured (\_ -> NothingHappenned) laneId
            )

        ErrorOccured error ->
            ( { model | error = error }, Cmd.none )

        NothingHappenned ->
            ( model, Cmd.none )

        EntryDropped ddMsg ->
            case model.dragDrop.draggedEntry of
                Nothing ->
                    ( { model | dragDrop = DD.update ddMsg model.dragDrop }, Cmd.none )

                Just entryId ->
                    let
                        index =
                            model.dragDrop.overEntry

                        laneId =
                            model.dragDrop.overLane

                        lanes =
                            Dict.map
                                (\_ lane ->
                                    let
                                        l =
                                            Lane.remove entryId lane
                                    in
                                    if Just l.id == laneId then
                                        if isJust index then
                                            Lane.insert (index |> Maybe.withDefault 0) entryId l

                                        else
                                            Lane.append entryId l

                                    else
                                        l
                                )
                                model.lanes
                    in
                    ( { model | lanes = lanes, dragDrop = DD.update ddMsg model.dragDrop }, Cmd.none )

        DragDropMsg ddMsg ->
            ( { model | dragDrop = DD.update ddMsg model.dragDrop }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "flex" ]
            [ div [ class "p-1 flex-1" ] [ h1 [ class "text-xl" ] [ text "Mini Kanban" ] ]
            , div [ class "p-1" ] [ button [ class "p-1", onClick AddLane ] [ text "➕" ] ]
            ]
        , div [ class "flex" ] (Dict.values model.lanes |> map (viewLane model.dragDrop model.entries))
        , div [] [ div [ style "color" "red" ] [ text model.error ] ]
        ]


viewLane : DD.Model -> EntriesCollection -> Lane.Lane -> Html Msg
viewLane ddModel entries lane =
    let
        viewEntries =
            lane.entries
                |> map (\id -> EntriesCollection.getEntry id entries)
                |> List.foldr (\e es -> e |> Maybe.map List.singleton |> Maybe.withDefault [] |> (\e_ -> e_ ++ es)) []
                |> List.indexedMap (viewEntry ddModel lane.id)
    in
    div
        (class "flex-1 border rounded m-1 pb-12"
            :: DD.onLaneEnter DragDropMsg lane.id
            ++ DD.onLaneOver NothingHappenned
            ++ DD.onLaneLeave DragDropMsg ddModel lane.id
            ++ DD.onEntryDrop EntryDropped
        )
        (div [ class "flex" ]
            [ input [ value lane.title, onInput (LaneTitleChanged lane.id), class "flex-grow text-xl p-1" ] []
            , button [ onClick (LaneDeleted lane.id) ] [ text "🗑️" ]
            ]
            :: viewEntries
        )


viewEntry : DD.Model -> Lane.ID -> Int -> Entry -> Html Msg
viewEntry ddModel laneId index entry =
    let
        dropIndicator =
            if ddModel.overEntry == Just index && ddModel.overLane == Just laneId then
                [ style "border-top" "4px solid blue" ]

            else
                []

        pointerEvents =
            if isJust ddModel.draggedEntry then
                [ style "pointer-events" "none" ]

            else
                []
    in
    div
        ([ draggable "true"
         , class "border rounded border-gray-300 m-1 p-1"
         ]
            ++ dropIndicator
            ++ DD.onEntryDragStart DragDropMsg entry.id
            ++ DD.onEntryEnter DragDropMsg index
            ++ DD.onEntryLeave DragDropMsg
        )
        [ div (class "w-full flex" :: pointerEvents)
            [ input [ value entry.title, onInput (EntryTitleChanged entry.id), class "flex-grow" ] []
            , button [ onClick (EntryDeleted entry.id) ] [ text "🗑️" ]
            ]
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


type alias LanesRepo err msg =
    { create : (err -> msg) -> (Lane.Lane -> msg) -> Cmd msg
    , update : (err -> msg) -> (Lane.Lane -> msg) -> Lane.Lane -> Cmd msg
    , delete : (err -> msg) -> (Lane.Lane -> msg) -> Lane.ID -> Cmd msg
    , getAll : (err -> msg) -> (LanesCollection.LanesCollection -> msg) -> Cmd msg
    }
