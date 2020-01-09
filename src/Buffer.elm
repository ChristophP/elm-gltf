module Buffer exposing (..)

import Bytes
import Bytes.Decode as BD
import Bytes.Decode.Extra as BDE


decodeIndices : BD.Decoder (List Int)
decodeIndices =
    BDE.withOffset 76768 <| BDE.list 12636 (BD.unsignedInt16 Bytes.LE)
