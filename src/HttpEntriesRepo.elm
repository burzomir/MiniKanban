module HttpEntriesRepo exposing (httpEntriesRepo)

import App exposing (EntriesRepo)
import EntriesCollection
import Entry
import Http


type alias BaseURL =
    String


entriesURL : BaseURL -> String
entriesURL baseURL =
    baseURL ++ "/entries"


httpEntriesRepo : BaseURL -> EntriesRepo String msg
httpEntriesRepo baseURL =
    { create = createEntry baseURL
    , update = updateEntry baseURL
    , delete = deleteEntryFromApi baseURL
    , getAll = getAllEntries baseURL
    }


createEntry : BaseURL -> (String -> msg) -> (Entry.Entry -> msg) -> Cmd msg
createEntry baseUrl errMsg okMsg =
    Http.post
        { url = baseUrl |> entriesURL
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


getAllEntries : BaseURL -> (String -> msg) -> (EntriesCollection.EntriesCollection -> msg) -> Cmd msg
getAllEntries baseURL errMsg okMsg =
    Http.get
        { url = baseURL |> entriesURL
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


updateEntry : BaseURL -> (String -> msg) -> (Entry.Entry -> msg) -> Entry.Entry -> Cmd msg
updateEntry baseURL errMsg okMsg entry =
    Http.request
        { url = (baseURL |> entriesURL) ++ entry.id
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


deleteEntryFromApi : BaseURL -> (String -> msg) -> (Entry.Entry -> msg) -> Entry.ID -> Cmd msg
deleteEntryFromApi baseURL errMsg okMsg id =
    Http.request
        { url = (baseURL |> entriesURL) ++ id
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
