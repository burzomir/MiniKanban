module Entry exposing (Entry, ID, Status(..), Title, decode, encode)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))


type Status
    = Todo
    | InProgress
    | Done


type alias Title =
    String


type alias ID =
    String


type alias Entry =
    { id : ID, title : Title, status : Status }


decode : Json.Decode.Decoder Entry
decode =
    Json.Decode.map3 Entry idDecoder titleDecoder statusDecoder


encode : Entry -> Json.Encode.Value
encode entry =
    Json.Encode.object
        [ ( "title", Json.Encode.string entry.title )
        , ( "status", statusEncoder entry.status )
        ]


statusEncoder : Status -> Json.Encode.Value
statusEncoder status =
    let
        s =
            case status of
                Todo ->
                    "Todo"

                InProgress ->
                    "InProgress"

                Done ->
                    "Done"
    in
    Json.Encode.string s


idDecoder : Json.Decode.Decoder ID
idDecoder =
    Json.Decode.field "id" Json.Decode.string


titleDecoder : Json.Decode.Decoder Title
titleDecoder =
    Json.Decode.field "title" Json.Decode.string


statusDecoder : Json.Decode.Decoder Status
statusDecoder =
    let
        decoder =
            Json.Decode.string
                |> Json.Decode.andThen
                    (\s ->
                        case s of
                            "Todo" ->
                                Json.Decode.succeed Todo

                            "InProgress" ->
                                Json.Decode.succeed InProgress

                            "Done" ->
                                Json.Decode.succeed Done

                            somethingElse ->
                                Json.Decode.fail <| "Unknown status: " ++ somethingElse
                    )
    in
    Json.Decode.field "status" decoder
