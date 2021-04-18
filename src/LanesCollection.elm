module LanesCollection exposing (..)

import Dict exposing (Dict)
import Entry
import Json.Decode
import Json.Encode
import Lane exposing (ID, Lane, Title)
import Maybe exposing (Maybe(..))


type alias LanesCollection =
    Dict ID Lane


empty : LanesCollection
empty =
    Dict.empty


insert : Lane -> LanesCollection -> LanesCollection
insert lane =
    Dict.insert lane.id lane


changeTitle : ID -> Title -> LanesCollection -> LanesCollection
changeTitle id title =
    Dict.update id (Maybe.map (\v -> { v | title = title }))


appendEntry : ID -> Entry.ID -> LanesCollection -> LanesCollection
appendEntry laneId entryId collection =
    Dict.update laneId (Maybe.map (Lane.append entryId)) collection


delete : ID -> LanesCollection -> LanesCollection
delete =
    Dict.remove


get : ID -> LanesCollection -> Maybe Lane
get =
    Dict.get


decode : Json.Decode.Decoder LanesCollection
decode =
    Json.Decode.list Lane.decode
        |> Json.Decode.map (List.map (\lane -> ( lane.id, lane )))
        |> Json.Decode.map Dict.fromList


encode : LanesCollection -> Json.Encode.Value
encode collection =
    Json.Encode.list Lane.encode <| Dict.values collection
