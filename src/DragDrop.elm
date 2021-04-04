module DragDrop exposing (Model, Msg, init, onEntryDragStart, onEntryDrop, onEntryEnter, onEntryLeave, onLaneEnter, onLaneLeave, onLaneOver, update)

import Entry
import Html
import Html.Events exposing (on, preventDefaultOn, stopPropagationOn)
import Json.Decode
import Lane
import Maybe.Extra exposing (isJust)


type alias Model =
    { draggedEntry : Maybe Entry.ID
    , overLane : Maybe Lane.ID
    , overEntry : Maybe Int
    }


type Msg
    = EntryDragged Entry.ID
    | LaneEntered Lane.ID
    | LaneLeft Lane.ID
    | EntryEntered Int
    | EntryLeft
    | EntryDropped


init : Model
init =
    { draggedEntry = Nothing
    , overLane = Nothing
    , overEntry = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        EntryDragged entryId ->
            { model | draggedEntry = Just entryId }

        LaneEntered laneId ->
            { model | overLane = Just laneId }

        LaneLeft laneId ->
            { model
                | overLane =
                    model.overLane
                        |> Maybe.andThen
                            (\id ->
                                if id == laneId then
                                    Nothing

                                else
                                    Just id
                            )
            }

        EntryEntered index ->
            { model | overEntry = Just index }

        EntryLeft ->
            { model | overEntry = Nothing }

        EntryDropped ->
            init


onEntryDragStart : (Msg -> msg) -> Entry.ID -> List (Html.Attribute msg)
onEntryDragStart msg entryId =
    [ on "dragstart" (Json.Decode.succeed <| msg (EntryDragged entryId)) ]


onLaneEnter : (Msg -> msg) -> Lane.ID -> List (Html.Attribute msg)
onLaneEnter msg laneId =
    [ on "dragenter" (Json.Decode.succeed <| msg (LaneEntered laneId)) ]


onLaneOver : msg -> List (Html.Attribute msg)
onLaneOver msg =
    [ preventDefaultOn "dragover" (Json.Decode.succeed ( msg, True )) ]


onLaneLeave : (Msg -> msg) -> Model -> Lane.ID -> List (Html.Attribute msg)
onLaneLeave msg model laneId =
    if isJust model.overEntry then
        []

    else
        [ on "dragleave" (Json.Decode.succeed <| msg <| LaneLeft laneId)
        ]


onEntryEnter : (Msg -> msg) -> Int -> List (Html.Attribute msg)
onEntryEnter msg index =
    [ stopPropagationOn "dragenter" (Json.Decode.succeed ( msg <| EntryEntered index, True ))
    ]


onEntryLeave : (Msg -> msg) -> List (Html.Attribute msg)
onEntryLeave msg =
    [ stopPropagationOn "dragleave" (Json.Decode.succeed ( msg EntryLeft, True ))
    ]


onEntryDrop : (Msg -> msg) -> List (Html.Attribute msg)
onEntryDrop msg =
    [ preventDefaultOn "drop" (Json.Decode.succeed ( msg EntryDropped, True )) ]
