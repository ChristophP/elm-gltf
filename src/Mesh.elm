module Mesh exposing (..)

import Buffer
import Bytes.Decode as BD
import Util exposing (listGetAt)
import WebGL


type alias Attributes =
    { normal : Int
    , position : Int
    }


type MeshMode
    = Triangles


type alias Mesh =
    { attributes : Attributes
    , indices : Maybe Int
    , mode : MeshMode
    }


extractMesh : List Buffer.ResolvedAccessor -> Mesh -> Maybe (WebGL.Mesh Attributes)
extractMesh resAccessors meshData =
    -- TODO make sure to handle different shapes of attributes in the future
    let
        -- look at mesh attributes, for each one decode a buffer to some list of values
        positions =
            []

        normals =
            []

        attributes =
            []

        maybeIndices =
            meshData.indices
                |> Maybe.andThen (\index -> listGetAt index resAccessors)
                |> Maybe.andThen
                    (\({ buffer } as accessor) ->
                        BD.decode (Buffer.indicesDecoder accessor) buffer
                    )
    in
    -- then take those value lists and turn them into a proper mesh
    case maybeIndices of
        Just indices ->
            Just (WebGL.indexedTriangles [] indices)

        Nothing ->
            Just (WebGL.triangles [])
