module Mesh exposing (..)

import Buffer
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


extractMesh : List Buffer.ResolvedAccessor -> Mesh -> WebGL.Mesh Attributes
extractMesh resAccessor meshData =
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
            Just []
    in
    -- then take those value lists and turn them into a proper mesh
    case maybeIndices of
        Just indices ->
            WebGL.indexedTriangles [] indices

        Nothing ->
            WebGL.triangles []
