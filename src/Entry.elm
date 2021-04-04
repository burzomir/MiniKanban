module Entry exposing (Entry, ID, Title, decode, encode)

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
        [ ( "title", Json.Encode.string entry.title )
        ]


idDecoder : Json.Decode.Decoder ID
idDecoder =
    Json.Decode.field "id" Json.Decode.string


titleDecoder : Json.Decode.Decoder Title
titleDecoder =
    Json.Decode.field "title" Json.Decode.string
