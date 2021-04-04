module App exposing (EntriesRepo, run)

import Browser
import Dict exposing (Dict)
import DragDrop as DD
import EntriesCollection exposing (EntriesCollection)
import Entry exposing (Entry, ID, Title)
import Html exposing (Html, button, div, h3, input, text)
import Html.Attributes exposing (class, draggable, style, title, value)
import Html.Events exposing (onClick, onInput)
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
    , dragDrop : DD.Model
    }


type Msg
    = Initialized EntriesCollection.EntriesCollection
    | AddEntry
    | EntryAdded Entry
    | EntryTitleChanged ID Title
    | EntryDeleted ID
    | ErrorOccured String
    | NothingHappenned
    | DragDropMsg DD.Msg
    | EntryDropped DD.Msg


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
    , dragDrop = DD.init
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

        EntryDeleted id ->
            ( { model | entries = EntriesCollection.deleteEntry id model.entries, error = "" }
            , repo.delete ErrorOccured (\_ -> NothingHappenned) id
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
                            model.dragDrop.overEntry |> Maybe.withDefault 0

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
                                        Lane.insert index entryId l

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
    let
        buttons =
            [ div [ class "p-1" ] [ button [ class "ring rounded-md font-semibold text-white bg-blue-500 ring p-1", onClick AddEntry ] [ text "Add" ] ] ]

        errors =
            [ div [ style "color" "red" ] [ text model.error ] ]
    in
    div []
        [ div [ class "flex" ] (Dict.values model.lanes |> map (viewLane model.dragDrop model.entries))
        , div [] <| buttons ++ errors
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
        (class "flex-1"
            :: DD.onLaneEnter DragDropMsg lane.id
            ++ DD.onLaneOver NothingHappenned
            ++ DD.onLaneLeave DragDropMsg ddModel lane.id
            ++ DD.onEntryDrop EntryDropped
        )
        (h3 [ class "text-xl p-1" ] [ text lane.title ] :: viewEntries)


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
