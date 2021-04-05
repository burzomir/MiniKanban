module EntriesCollection exposing (EntriesCollection, changeEntryTitle, decode, deleteEntry, empty, encode, getEntry, insertEntry)

import Dict exposing (Dict)
import Entry exposing (Entry, ID, Title)
import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))


type alias EntriesCollection =
    Dict ID Entry


empty : EntriesCollection
empty =
    Dict.empty


insertEntry : Entry -> EntriesCollection -> EntriesCollection
insertEntry entry =
    Dict.insert entry.id entry


changeEntryTitle : ID -> Title -> EntriesCollection -> EntriesCollection
changeEntryTitle id title =
    Dict.update id (Maybe.map (\v -> { v | title = title }))


deleteEntry : ID -> EntriesCollection -> EntriesCollection
deleteEntry =
    Dict.remove


getEntry : ID -> EntriesCollection -> Maybe Entry
getEntry =
    Dict.get


decode : Json.Decode.Decoder EntriesCollection
decode =
    Json.Decode.list Entry.decode
        |> Json.Decode.map (List.map (\entry -> ( entry.id, entry )))
        |> Json.Decode.map Dict.fromList


encode : EntriesCollection -> Json.Encode.Value
encode collection =
    Json.Encode.list Entry.encode <| Dict.values collection
