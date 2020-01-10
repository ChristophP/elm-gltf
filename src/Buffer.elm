module Buffer exposing (..)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Decode.Extra as BDE


type alias Buffer =
    { byteLength : Int
    , uri : Uri
    }


type Uri
    = DataUri String
    | RemoteUri String


toBytes : Uri -> Maybe Bytes
toBytes uri =
    let
        uriString =
            case uri of
                DataUri str ->
                    str

                RemoteUri _ ->
                    Debug.todo "Implement Me"
    in
    uriString
        |> String.dropLeft (String.length "data:application/octet-stream;base64,")
        |> Base64.toBytes


decodeIndices : BD.Decoder (List Int)
decodeIndices =
    BDE.withOffset 76768 <| BDE.list 12636 (BD.unsignedInt16 Bytes.LE)
