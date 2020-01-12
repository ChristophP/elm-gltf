module Mesh exposing (..)

import Buffer
import Bytes.Decode as BD
import Math.Vector3 exposing (Vec3)
import Util exposing (listGetAt, listToTriples)
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


type alias PositionNormalAttributes =
    { position : Vec3
    , normal : Vec3
    }


extractMesh : List Buffer.ResolvedAccessor -> Mesh -> Maybe (WebGL.Mesh PositionNormalAttributes)
extractMesh resAccessors meshData =
    -- TODO make sure to handle different shapes of attributes in the future
    let
        -- look at mesh attributes, for each one decode a buffer to some list of values
        positions =
            listGetAt meshData.attributes.position resAccessors
                |> Maybe.andThen
                    (\({ buffer } as accessor) ->
                        BD.decode (Buffer.vec3ListDecoder accessor) buffer
                    )

        normals =
            listGetAt meshData.attributes.normal resAccessors
                |> Maybe.andThen
                    (\({ buffer } as accessor) ->
                        BD.decode (Buffer.vec3ListDecoder accessor) buffer
                    )

        maybeAttributes =
            Maybe.map2
                (List.map2 PositionNormalAttributes)
                positions
                normals

        maybeIndices =
            meshData.indices
                |> Maybe.andThen (\index -> listGetAt index resAccessors)
                |> Maybe.andThen
                    (\({ buffer } as accessor) ->
                        BD.decode (Buffer.intTriplesDecoder accessor) buffer
                    )
    in
    -- then take those value lists and turn them into a proper mesh
    case maybeIndices of
        Just indices ->
            Maybe.map (\attrs -> WebGL.indexedTriangles attrs indices)
                maybeAttributes

        Nothing ->
            maybeAttributes
                |> Maybe.andThen listToTriples
                |> Maybe.map WebGL.triangles
