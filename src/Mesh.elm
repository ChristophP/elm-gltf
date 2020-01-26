module Mesh exposing (..)

import Buffer
import Bytes.Decode as BD
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Util exposing (listGetAt, listToTriples)
import WebGL


type alias Attributes =
    { normal : Int
    , position : Int
    , texCoords : Int
    }


type MeshMode
    = Triangles


type alias Mesh =
    { attributes : Attributes
    , indices : Maybe Int
    , mode : MeshMode
    , material : Int
    }



-- TODO add support for "at least two textures" according to spec
-- https://github.com/KhronosGroup/glTF/tree/master/specification/2.0#meshes


type alias PositionNormalTexCoordsAttributes =
    { position : Vec3
    , normal : Vec3
    , texCoords : Vec2
    }


extractMesh :
    List Buffer.ResolvedAccessor
    -> Mesh
    -> Maybe (WebGL.Mesh PositionNormalTexCoordsAttributes)
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

        texCoords =
            listGetAt meshData.attributes.texCoords resAccessors
                |> Maybe.andThen
                    (\({ buffer } as accessor) ->
                        BD.decode (Buffer.vec2ListDecoder accessor) buffer
                    )

        maybeAttributes =
            Maybe.map3
                (List.map3 PositionNormalTexCoordsAttributes)
                positions
                normals
                texCoords

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
