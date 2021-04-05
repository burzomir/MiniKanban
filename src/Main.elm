port module Main exposing (..)

import App
import Browser
import EntriesCollection
import Html exposing (Html)
import Json.Decode
import Json.Encode
import LanesCollection
import LocalStorageEntriesRepo exposing (localStorageEntriesRepo)
import LocalStorageLanesRepo exposing (localStorageLanesRepo)


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { app : App.Model
    , entries : EntriesCollection.EntriesCollection
    , lanes : LanesCollection.LanesCollection
    }


type alias Flags =
    { entries : EntriesCollection.EntriesCollection
    , lanes : LanesCollection.LanesCollection
    }


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map2
        Flags
        (Json.Decode.field "entries" EntriesCollection.decode)
        (Json.Decode.field "lanes" LanesCollection.decode)


init : Json.Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decodedFlags =
            Json.Decode.decodeValue flagsDecoder flags
                |> Result.withDefault { entries = EntriesCollection.empty, lanes = LanesCollection.empty }

        env =
            { entriesRepo = localStorageEntriesRepo saveEntriesCollection decodedFlags.entries
            , lanesRepo = localStorageLanesRepo saveLanesCollection decodedFlags.lanes
            }

        ( app, appCmd ) =
            App.init env ()

        model =
            { app = app, entries = decodedFlags.entries, lanes = decodedFlags.lanes }
    in
    ( model, Cmd.batch [ appCmd |> Cmd.map AppMsg ] )


type Msg
    = AppMsg App.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppMsg appMsg ->
            let
                env =
                    { entriesRepo = localStorageEntriesRepo saveEntriesCollection model.app.entries
                    , lanesRepo = localStorageLanesRepo saveLanesCollection model.app.lanes
                    }

                ( app, cmd ) =
                    App.update env appMsg model.app
            in
            ( { model | app = app }, cmd |> Cmd.map AppMsg )


view : Model -> Html Msg
view model =
    App.view model.app |> Html.map AppMsg


port saveEntriesCollection : Json.Encode.Value -> Cmd msg


port saveLanesCollection : Json.Encode.Value -> Cmd msg
