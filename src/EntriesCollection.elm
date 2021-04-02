module EntriesCollection exposing (EntriesCollection, changeEntryStatus, changeEntryTitle, decode, deleteEntry, getEntry, insertEntry)

import Dict exposing (Dict)
import Entry exposing (Entry, ID, Status, Title)
import Json.Decode
import Maybe exposing (Maybe(..))


type alias EntriesCollection =
    Dict ID Entry


insertEntry : Entry -> EntriesCollection -> EntriesCollection
insertEntry entry =
    Dict.insert entry.id entry


changeEntryTitle : ID -> Title -> EntriesCollection -> EntriesCollection
changeEntryTitle id title =
    Dict.update id (Maybe.map (\v -> { v | title = title }))


changeEntryStatus : ID -> Status -> EntriesCollection -> EntriesCollection
changeEntryStatus id status =
    Dict.update id (Maybe.map (\v -> { v | status = status }))


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
