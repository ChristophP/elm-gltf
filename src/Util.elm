module Util exposing (defaultDecoder, toIndexedList)

import Json.Decode as JD


toIndexedList : List a -> List ( Int, a )
toIndexedList =
    List.indexedMap Tuple.pair


defaultDecoder : a -> JD.Decoder a -> JD.Decoder a
defaultDecoder default decoder =
    JD.oneOf [ decoder, JD.succeed default ]
