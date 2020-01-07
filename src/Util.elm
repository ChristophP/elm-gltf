module Util exposing (toIndexedList)


toIndexedList : List a -> List ( Int, a )
toIndexedList =
    List.indexedMap Tuple.pair
