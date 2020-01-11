module GLTF exposing (..)

import Buffer exposing (Accessor, Buffer, BufferView)
import Bytes exposing (Bytes)
import Bytes.Extra as BE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Math.Matrix4 as Mat4
import Mesh exposing (Attributes, Mesh)
import Set
import Util exposing (defaultDecoder, listGetAt, maybeSequence)
import WebGL


type GLTF
    = GLTF
        { version : String
        , defaultScene : Int
        , scenes : List Scene
        , cameras : List Camera
        , nodes : List Node
        , meshes : List Mesh
        , accessors : List Accessor
        , bufferViews : List BufferView
        , buffers : List Buffer
        }


type Scene
    = Scene (List Int)


type Camera
    = Perspective { aspectRatio : Float, yfov : Float, zfar : Maybe Float, znear : Float }



--| Orthographic { xmag : Float, ymag : Float, zfar : Float, znear : Float }


type alias Transformation =
    { ix : Float
    , iy : Float
    , iz : Float
    , jx : Float
    , jy : Float
    , jz : Float
    , kx : Float
    , ky : Float
    , kz : Float
    , px : Float
    , py : Float
    , pz : Float
    , scale : Float

    --, isRightHanded : Bool
    }


type Node
    = Node Mat4.Mat4 { mesh : Maybe Int, camera : Maybe Int }
    | Group Mat4.Mat4 (List Node)


type alias RawNode =
    { matrix : Mat4.Mat4
    , mesh : Maybe Int
    , camera : Maybe Int
    , children : List Int
    }



-- getters


getDefaultScene : GLTF -> Maybe Scene
getDefaultScene (GLTF data) =
    listGetAt data.defaultScene data.scenes


getScenes : GLTF -> List Scene
getScenes (GLTF data) =
    data.scenes


getCameras : GLTF -> List Camera
getCameras (GLTF data) =
    data.cameras



-- decoders


defaultSceneDecoder : JD.Decoder Int
defaultSceneDecoder =
    JD.field "scene" JD.int


scenesDecoder : JD.Decoder (List Scene)
scenesDecoder =
    JD.field "scenes"
        (JD.list
            (JD.field "nodes"
                (JD.list JD.int)
                |> JD.map Scene
            )
        )


perspectiveDecoder : JD.Decoder Camera
perspectiveDecoder =
    JD.field "perspective"
        (JD.map4
            (\aspectRatio yfov zfar znear ->
                Perspective
                    { aspectRatio = aspectRatio
                    , yfov = yfov
                    , zfar = zfar
                    , znear = znear
                    }
            )
            (JD.field "aspectRatio" JD.float)
            (JD.field "yfov" JD.float)
            (JD.field "zfar" (JD.maybe JD.float))
            (JD.field "znear" JD.float)
        )


orthographicDecoder : JD.Decoder Camera
orthographicDecoder =
    JD.lazy (\() -> Debug.todo "Add orthographic")


camerasDecoder : JD.Decoder (List Camera)
camerasDecoder =
    JD.field "cameras"
        (JD.list
            (JD.field "type" JD.string
                |> JD.andThen
                    (\string ->
                        case string of
                            "perspective" ->
                                perspectiveDecoder

                            "orthographic" ->
                                orthographicDecoder

                            _ ->
                                JD.fail "Could not decode a camera object"
                    )
            )
        )


matrixDecoder : JD.Decoder Mat4.Mat4
matrixDecoder =
    JD.list JD.float
        |> JD.andThen
            (\list ->
                case list of
                    [ ix, iy, iz, iw, jx, jy, jz, jw, kx, ky, kz, kw, px, py, pz, pw ] ->
                        JD.succeed <|
                            Mat4.fromRecord
                                { m11 = ix
                                , m21 = iy
                                , m31 = iz
                                , m41 = iw
                                , m12 = jx
                                , m22 = jy
                                , m32 = jz
                                , m42 = jw
                                , m13 = kx
                                , m23 = ky
                                , m33 = kz
                                , m43 = kw
                                , m14 = px
                                , m24 = py
                                , m34 = pz
                                , m44 = pw
                                }

                    _ ->
                        JD.fail "Matrix did not have the correct number of 16 entries"
            )


getRootNodes : List ( Int, RawNode ) -> List RawNode
getRootNodes indexedNodes =
    let
        childrenIndices =
            List.concatMap (Tuple.second >> .children) indexedNodes
                |> Set.fromList
    in
    List.filterMap
        (\( index, node ) ->
            -- if node is not anyone's child it is a root node
            if Set.member index childrenIndices then
                Nothing

            else
                Just node
        )
        indexedNodes


buildTreeFromRootNodesHelp : List ( Int, RawNode ) -> RawNode -> Node
buildTreeFromRootNodesHelp allNodes rootNode =
    case rootNode.children of
        [] ->
            Node rootNode.matrix
                { mesh = rootNode.mesh
                , camera = rootNode.camera
                }

        children ->
            let
                childNodes =
                    List.filterMap
                        (\( index, node ) ->
                            if List.member index rootNode.children then
                                Just (buildTreeFromRootNodesHelp allNodes node)

                            else
                                Nothing
                        )
                        allNodes
            in
            -- TODO find children via indices and pass them to the group
            Group rootNode.matrix childNodes


buildTreeFromRootNodes : List RawNode -> List Node
buildTreeFromRootNodes rawNodes =
    let
        indexedNodes =
            Util.toIndexedList rawNodes

        rootNodes =
            getRootNodes indexedNodes
    in
    List.map
        (buildTreeFromRootNodesHelp indexedNodes)
        rootNodes


nodesDecoder : JD.Decoder (List Node)
nodesDecoder =
    JD.field "nodes"
        (JD.list
            (JD.map4
                (\children meshIndex cameraIndex matrix ->
                    RawNode matrix meshIndex cameraIndex children
                )
                (JD.maybe (JD.field "children" (JD.list JD.int))
                    |> JD.map (Maybe.withDefault [])
                )
                (JD.maybe (JD.field "mesh" JD.int))
                (JD.maybe (JD.field "camera" JD.int))
                (defaultDecoder Mat4.identity
                    (JD.field "matrix" matrixDecoder)
                )
            )
        )
        |> JD.map buildTreeFromRootNodes


meshModeDecoder : JD.Decoder Mesh.MeshMode
meshModeDecoder =
    JD.int
        |> JD.andThen
            (\int ->
                case int of
                    4 ->
                        JD.succeed Mesh.Triangles

                    _ ->
                        JD.fail ("Unknown Mesh Mode constant: Got " ++ String.fromInt int)
            )


meshesDecoder : JD.Decoder (List Mesh)
meshesDecoder =
    let
        attributesDecoder =
            JD.map2 Attributes
                (JD.field "NORMAL" JD.int)
                (JD.field "POSITION" JD.int)
    in
    JD.field "meshes"
        (JD.list
            (JD.field "primitives"
                (JD.list
                    (JD.map3 Mesh
                        (JD.field "attributes" attributesDecoder)
                        (JD.maybe (JD.field "indices" JD.int))
                        (JD.field "mode" meshModeDecoder)
                    )
                )
            )
        )
        |> JD.map List.concat


componentTypeDecoder : JD.Decoder Buffer.ComponentType
componentTypeDecoder =
    JD.int
        |> JD.andThen
            (\int ->
                case int of
                    5120 ->
                        JD.succeed Buffer.Byte

                    5121 ->
                        JD.succeed Buffer.UnsignedByte

                    5122 ->
                        JD.succeed Buffer.Short

                    5123 ->
                        JD.succeed Buffer.UnsignedShort

                    5125 ->
                        JD.succeed Buffer.UnsignedInt

                    5126 ->
                        JD.succeed Buffer.Float

                    _ ->
                        JD.fail
                            ("Found unknown component type constant: Got " ++ String.fromInt int)
            )


structureTypeDecoder : JD.Decoder Buffer.StructureType
structureTypeDecoder =
    JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "SCALAR" ->
                        JD.succeed Buffer.Scalar

                    "VEC2" ->
                        JD.succeed Buffer.Vec2

                    "VEC3" ->
                        JD.succeed Buffer.Vec3

                    "Vec4" ->
                        JD.succeed Buffer.Vec4

                    "MAT2" ->
                        JD.succeed Buffer.Mat2

                    "MAT3" ->
                        JD.succeed Buffer.Mat3

                    "MAT4" ->
                        JD.succeed Buffer.Mat4

                    _ ->
                        JD.fail
                            ("Found unknown numComponet constant: Got " ++ type_)
            )


minMaxDecoder : JD.Decoder ( List Float, List Float )
minMaxDecoder =
    JD.map2 Tuple.pair
        (JD.field "min" (JD.list JD.float))
        (JD.field "max" (JD.list JD.float))


accessorsDecoder : JD.Decoder (List Accessor)
accessorsDecoder =
    JD.field "accessors"
        (JD.list
            (JD.map6 Accessor
                (JD.field "bufferView" JD.int)
                (JD.field "byteOffset" JD.int)
                (JD.field "componentType" componentTypeDecoder)
                (JD.field "count" JD.int)
                minMaxDecoder
                (JD.field "type" structureTypeDecoder)
            )
        )



-- buffer views


targetDecoder : JD.Decoder Buffer.BufferType
targetDecoder =
    JD.int
        |> JD.andThen
            (\int ->
                case int of
                    34962 ->
                        JD.succeed Buffer.ArrayBuffer

                    34963 ->
                        JD.succeed Buffer.ElementArrayBuffer

                    _ ->
                        JD.fail
                            ("Invalid buffer type constant: Got " ++ String.fromInt int)
            )


bufferViewsDecoder : JD.Decoder (List BufferView)
bufferViewsDecoder =
    JD.field "bufferViews"
        (JD.list
            (JD.map5 BufferView
                (JD.field "buffer" JD.int)
                (JD.field "byteOffset" (defaultDecoder 0 JD.int))
                (JD.field "byteLength" JD.int)
                (JD.maybe (JD.field "byteStride" JD.int))
                (JD.maybe (JD.field "target" targetDecoder))
            )
        )



-- buffers


uriDecoder : JD.Decoder Buffer.Uri
uriDecoder =
    JD.string
        |> JD.map
            (\uri ->
                if String.startsWith "data:" uri then
                    Buffer.DataUri uri

                else
                    Buffer.RemoteUri uri
            )


buffersDecoder : JD.Decoder (List Buffer)
buffersDecoder =
    JD.field "buffers"
        (JD.list
            (JD.map2
                Buffer
                (JD.field "byteLength" JD.int)
                (JD.field "uri"
                    uriDecoder
                )
            )
        )


gltfEmbeddedDecoder : JD.Decoder GLTF
gltfEmbeddedDecoder =
    JD.succeed
        (\version scene scenes cameras nodes meshes accessors bufferViews buffers ->
            GLTF
                { version = version
                , defaultScene = scene
                , scenes = scenes
                , cameras = cameras
                , nodes = nodes
                , meshes = meshes
                , accessors = accessors
                , bufferViews = bufferViews
                , buffers = buffers
                }
        )
        |> JDP.required "asset" (JD.field "version" JD.string)
        |> JDP.custom defaultSceneDecoder
        |> JDP.custom scenesDecoder
        |> JDP.custom camerasDecoder
        |> JDP.custom nodesDecoder
        |> JDP.custom meshesDecoder
        |> JDP.custom accessorsDecoder
        |> JDP.custom bufferViewsDecoder
        |> JDP.custom buffersDecoder


resolveAccessors : GLTF -> Maybe (List Buffer.ResolvedAccessor)
resolveAccessors (GLTF gltf) =
    let
        maybeBuffers =
            List.foldr
                (\current seq ->
                    Maybe.map2 (::)
                        (Buffer.toBytes current.uri)
                        seq
                )
                (Just [])
                gltf.buffers

        toResolvedAccessor ( accessor, bufferView, buffer ) =
            { viewOffset = bufferView.byteOffset
            , componentType = accessor.componentType
            , count = accessor.count
            , minMax = accessor.minMax
            , type_ = accessor.type_
            , buffer = buffer
            , accessorOffset = accessor.byteOffset
            , byteStride = bufferView.byteStride
            , target = bufferView.target
            }

        collectData buffers accessor =
            listGetAt accessor.bufferView gltf.bufferViews
                |> Maybe.andThen
                    (\bufferView ->
                        listGetAt bufferView.buffer buffers
                            |> Maybe.map (\buffer -> ( accessor, bufferView, buffer ))
                    )
                |> Maybe.map toResolvedAccessor
    in
    maybeBuffers
        |> Maybe.andThen
            (\buffers ->
                List.map (collectData buffers) gltf.accessors
                    |> maybeSequence
            )
