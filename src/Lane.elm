module Lane exposing (..)

import Entry
import Json.Decode
import Json.Encode
import List exposing (drop, filter, take)


type alias ID =
    String


type alias Title =
    String


type alias Lane =
    { id : ID
    , title : Title
    , entries : List Entry.ID
    }


insert : Int -> Entry.ID -> Lane -> Lane
insert index id lane =
    let
        before =
            take index lane.entries

        after =
            drop index lane.entries

        entries =
            before ++ (id :: after)
    in
    { lane | entries = entries }


append : Entry.ID -> Lane -> Lane
append entryId lane =
    { lane | entries = lane.entries ++ [ entryId ] }


remove : Entry.ID -> Lane -> Lane
remove id lane =
    let
        entries =
            filter ((/=) id) lane.entries
    in
    { lane | entries = entries }


decode : Json.Decode.Decoder Lane
decode =
    Json.Decode.map3 Lane idDecoder titleDecoder entriesDecoder


encode : Lane -> Json.Encode.Value
encode lane =
    Json.Encode.object
        [ ( "id", Json.Encode.string lane.id )
        , ( "title", Json.Encode.string lane.title )
        , ( "entries", entriesEncoder lane.entries )
        ]


idDecoder : Json.Decode.Decoder ID
idDecoder =
    Json.Decode.field "id" Json.Decode.string


titleDecoder : Json.Decode.Decoder Title
titleDecoder =
    Json.Decode.field "title" Json.Decode.string


entriesDecoder : Json.Decode.Decoder (List Entry.ID)
entriesDecoder =
    Json.Decode.list Entry.idDecoder


entriesEncoder : List Entry.ID -> Json.Encode.Value
entriesEncoder =
    Json.Encode.list Entry.idEncoder
