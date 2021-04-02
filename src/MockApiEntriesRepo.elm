module MockApiEntriesRepo exposing (mockApiEntriesRepo)

import App exposing (EntriesRepo)
import EntriesCollection
import Entry
import Http



mockApiEntriesRepo : EntriesRepo String msg
mockApiEntriesRepo = 
    { create = createEntry
    , update = updateEntry
    , delete = deleteEntryFromApi
    , getAll = getAllEntries
    }


createEntry : (String -> msg) -> (Entry.Entry -> msg) -> Cmd msg
createEntry errMsg okMsg =
    Http.post
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos"
        , body = Http.emptyBody
        , expect = Http.expectJson (processEntryCreationResult errMsg okMsg) Entry.decode
        }


processEntryCreationResult : (String -> msg) -> (Entry.Entry -> msg) -> Result Http.Error Entry.Entry -> msg
processEntryCreationResult errMsg okMsg res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    errMsg d

                _ ->
                    errMsg "Some error occured"

        Ok entry ->
            okMsg entry


getAllEntries : (String -> msg) -> (EntriesCollection.EntriesCollection -> msg) -> Cmd msg
getAllEntries errMsg okMsg =
    Http.get
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos"
        , expect = Http.expectJson (processAllEntriesResult errMsg okMsg) EntriesCollection.decode
        }


processAllEntriesResult : (String -> msg) -> (EntriesCollection.EntriesCollection -> msg) -> Result Http.Error EntriesCollection.EntriesCollection -> msg
processAllEntriesResult errMsg okMsg res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    errMsg d

                _ ->
                    errMsg "Some error occured"

        Ok entries ->
            okMsg entries


updateEntry : (String -> msg) -> (Entry.Entry -> msg) -> Entry.Entry -> Cmd msg
updateEntry errMsg okMsg entry =
    Http.request
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos/" ++ entry.id
        , method = "PUT"
        , headers = []
        , body = Http.jsonBody <| Entry.encode entry
        , expect = Http.expectJson (processUpdateEntryResult errMsg okMsg) Entry.decode
        , timeout = Nothing
        , tracker = Nothing
        }


processUpdateEntryResult : (String -> msg) -> (Entry.Entry -> msg) -> Result Http.Error Entry.Entry -> msg
processUpdateEntryResult errMsg okMsg res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    errMsg d

                _ ->
                    errMsg "Some error occured"

        Ok entry ->
            okMsg entry


deleteEntryFromApi : (String -> msg) -> (Entry.Entry -> msg) -> Entry.ID -> Cmd msg
deleteEntryFromApi errMsg okMsg id =
    Http.request
        { url = "https://60662038b8fbbd0017568155.mockapi.io/todos/" ++ id
        , method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson (processDeleteEntryResult errMsg okMsg) Entry.decode
        , timeout = Nothing
        , tracker = Nothing
        }


processDeleteEntryResult : (String -> msg) -> (Entry.Entry -> msg) -> Result Http.Error Entry.Entry -> msg
processDeleteEntryResult errMsg okMsg res =
    case res of
        Err e ->
            case e of
                Http.BadBody d ->
                    errMsg d

                _ ->
                    errMsg "Some error occured"

        Ok entry ->
            okMsg entry
