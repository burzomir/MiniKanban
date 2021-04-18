module LocalStorageLanesRepo exposing (..)

import App
import Dict
import Json.Encode
import Lane
import LanesCollection
import List exposing (maximum)
import Maybe.Extra
import Task


localStorageLanesRepo : (Json.Encode.Value -> Cmd msg) -> LanesCollection.LanesCollection -> App.LanesRepo String msg
localStorageLanesRepo saveToLocalStorage collection =
    { create = create saveToLocalStorage collection
    , update = update saveToLocalStorage collection
    , delete = delete saveToLocalStorage collection
    , getAll =
        \_ okMsg ->
            okMsg collection |> msgToCmd
    }


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.succeed msg |> Task.perform identity


create : (Json.Encode.Value -> Cmd msg) -> LanesCollection.LanesCollection -> (err -> msg) -> (Lane.Lane -> msg) -> Cmd msg
create saveToLocalStorage collection _ okMsg =
    let
        id =
            Dict.values collection
                |> List.map (\l -> String.toInt l.id)
                |> Maybe.Extra.values
                |> maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 1
                |> String.fromInt

        lane =
            { id = id, title = "Lane " ++ id, entries = [] }

        newCollection =
            LanesCollection.insert lane collection

        save =
            newCollection |> LanesCollection.encode |> saveToLocalStorage

        return =
            lane |> okMsg |> msgToCmd
    in
    Cmd.batch
        [ save, return ]


update : (Json.Encode.Value -> Cmd msg) -> LanesCollection.LanesCollection -> (err -> msg) -> (Lane.Lane -> msg) -> Lane.Lane -> Cmd msg
update saveToLocalStorage collection _ okMsg lane =
    let
        newCollection =
            LanesCollection.update lane collection

        save =
            newCollection |> LanesCollection.encode |> saveToLocalStorage

        return =
            lane |> okMsg |> msgToCmd
    in
    Cmd.batch [ save, return ]


delete : (Json.Encode.Value -> Cmd msg) -> LanesCollection.LanesCollection -> (err -> msg) -> (Lane.Lane -> msg) -> Lane.ID -> Cmd msg
delete saveToLocalStorage collection _ okMsg id =
    let
        lane =
            LanesCollection.get id collection

        newCollection =
            LanesCollection.delete id collection

        save =
            newCollection |> LanesCollection.encode |> saveToLocalStorage

        return =
            lane |> Maybe.withDefault { id = "", title = "", entries = [] } |> okMsg |> msgToCmd
    in
    Cmd.batch [ save, return ]
