module Lane exposing (..)

import Entry
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
