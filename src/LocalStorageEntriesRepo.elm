module LocalStorageEntriesRepo exposing (..)

import App
import Debug
import Dict
import EntriesCollection
import Json.Encode
import List exposing (maximum)
import Maybe.Extra
import Task


localStorageEntriesRepo : (Json.Encode.Value -> Cmd msg) -> EntriesCollection.EntriesCollection -> App.EntriesRepo String msg
localStorageEntriesRepo saveToLocalStorage collection =
    { create =
        \_ okMsg ->
            let
                id =
                    Dict.values collection
                        |> List.map (\e -> String.toInt e.id)
                        |> Maybe.Extra.values
                        |> maximum
                        |> Maybe.map ((+) 1)
                        |> Maybe.withDefault 1
                        |> String.fromInt

                entry =
                    { id = id, title = "" }

                newCollection =
                    EntriesCollection.insertEntry entry collection

                save =
                    newCollection |> EntriesCollection.encode |> saveToLocalStorage

                return =
                    entry |> okMsg |> msgToCmd
            in
            Cmd.batch
                [ save, return ]
    , update =
        \_ okMsg entry ->
            let
                newCollection =
                    EntriesCollection.changeEntryTitle entry.id entry.title collection

                save =
                    newCollection |> EntriesCollection.encode |> saveToLocalStorage

                return =
                    entry |> okMsg |> msgToCmd
            in
            Cmd.batch [ save, return ]
    , delete =
        \_ okMsg id ->
            let
                entry =
                    EntriesCollection.getEntry id collection

                newCollection =
                    EntriesCollection.deleteEntry id collection

                save =
                    newCollection |> EntriesCollection.encode |> saveToLocalStorage

                return =
                    entry |> Maybe.withDefault { id = "", title = "" } |> okMsg |> msgToCmd
            in
            Cmd.batch [ save, return ]
    , getAll =
        \_ okMsg ->
            okMsg collection |> msgToCmd
    }


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.succeed msg |> Task.perform identity
