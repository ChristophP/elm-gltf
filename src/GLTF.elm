module GLTF exposing (..)

import Json.Decode as JD
import Math.Matrix4 as Mat4
import Set
import Util
import WebGL exposing (Mesh)


type GLTF
    = GLTF
        { version : String
        , defaultScene : Int
        , scenes : List Scene
        , cameras : List Camera
        , nodes : List Node
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



-- list find helper


listFind : Int -> List a -> Maybe a
listFind index list =
    listFindHelp index list 0


listFindHelp index list currentIndex =
    case list of
        head :: tail ->
            if currentIndex == index then
                Just head

            else
                listFindHelp index tail (currentIndex + 1)

        [] ->
            Nothing



-- getters


getDefaultScene : GLTF -> Maybe Scene
getDefaultScene (GLTF data) =
    listFind data.defaultScene data.scenes


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
                (JD.oneOf
                    [ JD.field "matrix" matrixDecoder
                    , JD.succeed Mat4.identity
                    ]
                )
            )
        )
        |> JD.map buildTreeFromRootNodes


gltfEmbeddedDecoder : JD.Decoder GLTF
gltfEmbeddedDecoder =
    JD.map5
        (\version scene scenes cameras nodes ->
            GLTF
                { version = version
                , defaultScene = scene
                , scenes = scenes
                , cameras = cameras
                , nodes = nodes
                }
        )
        (JD.field "asset" (JD.field "version" JD.string))
        defaultSceneDecoder
        scenesDecoder
        camerasDecoder
        nodesDecoder