module Entry exposing (Entry, ID, Title, decode, encode, decodeId, encodeId)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))


type alias Title =
    String


type alias ID =
    String


type alias Entry =
    { id : ID, title : Title }


decode : Json.Decode.Decoder Entry
decode =
    Json.Decode.map2 Entry idDecoder titleDecoder


encode : Entry -> Json.Encode.Value
encode entry =
    Json.Encode.object
        [ ( "id", Json.Encode.string entry.id )
        , ( "title", Json.Encode.string entry.title )
        ]


idDecoder : Json.Decode.Decoder ID
idDecoder =
    Json.Decode.field "id" Json.Decode.string


decodeId : Json.Decode.Decoder ID
decodeId =
    Json.Decode.string


encodeId : ID -> Json.Encode.Value
encodeId =
    Json.Encode.string


titleDecoder : Json.Decode.Decoder Title
titleDecoder =
    Json.Decode.field "title" Json.Decode.string
