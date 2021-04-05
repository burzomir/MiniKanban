port module Main exposing (..)

import App
import Browser
import EntriesCollection
import Html exposing (Html)
import Json.Decode
import Json.Encode
import LocalStorageEntriesRepo exposing (localStorageEntriesRepo)


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
    }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init entriesCollection =
    let
        entries =
            Json.Decode.decodeValue EntriesCollection.decode entriesCollection
                |> Result.withDefault EntriesCollection.empty

        repo =
            localStorageEntriesRepo saveEntriesCollection entries

        ( app, appCmd ) =
            App.init repo ()
    in
    ( { app = app, entries = entries }, Cmd.batch [ appCmd |> Cmd.map AppMsg ] )


type Msg
    = AppMsg App.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppMsg appMsg ->
            let
                repo =
                    localStorageEntriesRepo saveEntriesCollection model.app.entries

                ( app, cmd ) =
                    App.update repo appMsg model.app
            in
            ( { model | app = app }, cmd |> Cmd.map AppMsg )


view : Model -> Html Msg
view model =
    App.view model.app |> Html.map AppMsg


port saveEntriesCollection : Json.Encode.Value -> Cmd msg
