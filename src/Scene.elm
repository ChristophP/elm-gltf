module Scene exposing (Scene, fromGLTF, getCameras, getDrawables)

import GLTF
import Math.Matrix4 as Mat4
import Mesh
import Set
import Util exposing (listGetAt, maybeSequence)
import WebGL


type Node
    = MeshNode Mat4.Mat4 (WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes)
    | CameraNode Mat4.Mat4 GLTF.Camera
    | Group Mat4.Mat4 (List Node)


fromGLTF : GLTF.GLTF -> Maybe Scene
fromGLTF gltf =
    let
        maybeMeshes =
            GLTF.resolveAccessors gltf
                |> Maybe.andThen
                    (\accessors ->
                        List.map (Mesh.extractMesh accessors) (GLTF.getMeshes gltf)
                            |> maybeSequence
                    )

        rawNodes =
            GLTF.getNodes gltf

        cameras =
            GLTF.getCameras gltf

        indexedNodes =
            Util.toIndexedList rawNodes

        rootNodes =
            getRootNodes indexedNodes
    in
    Maybe.andThen
        (\meshes ->
            List.map
                (buildTreeHelp meshes cameras indexedNodes)
                rootNodes
                |> maybeSequence
        )
        maybeMeshes


type alias Scene =
    List Node


type alias Camera =
    GLTF.Camera


getDrawables : Scene -> List ( Mat4.Mat4, WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes )
getDrawables scene =
    getDrawablesHelp scene Mat4.identity []


getDrawablesHelp :
    List Node
    -> Mat4.Mat4
    -> List ( Mat4.Mat4, WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes )
    -> List ( Mat4.Mat4, WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes )
getDrawablesHelp scene currentTransform accumulated =
    List.concatMap
        (\node ->
            case node of
                Group matrix children ->
                    getDrawablesHelp children (Mat4.mul currentTransform matrix) accumulated

                CameraNode _ _ ->
                    accumulated

                MeshNode matrix mesh ->
                    ( Mat4.mul currentTransform matrix, mesh ) :: accumulated
        )
        scene


getCameras : Scene -> List ( Mat4.Mat4, Camera )
getCameras scene =
    getCamerasHelp scene Mat4.identity []


getCamerasHelp : List Node -> Mat4.Mat4 -> List ( Mat4.Mat4, Camera ) -> List ( Mat4.Mat4, Camera )
getCamerasHelp scene currentTransform accumulated =
    List.concatMap
        (\node ->
            case node of
                Group matrix children ->
                    getCamerasHelp children (Mat4.mul currentTransform matrix) accumulated

                CameraNode matrix camera ->
                    ( Mat4.mul currentTransform matrix, camera ) :: accumulated

                MeshNode _ _ ->
                    accumulated
        )
        scene



-- Nodes


getRootNodes : List ( Int, GLTF.Node ) -> List GLTF.Node
getRootNodes indexedNodes =
    let
        toChildIndices node =
            case node of
                GLTF.Group _ indices ->
                    indices

                _ ->
                    []

        childrenIndices =
            List.concatMap (Tuple.second >> toChildIndices) indexedNodes
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


buildTreeHelp :
    List (WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes)
    -> List Camera
    -> List ( Int, GLTF.Node )
    -> GLTF.Node
    -> Maybe Node
buildTreeHelp meshes cameras allNodes rootNode =
    case rootNode of
        GLTF.MeshNode matrix index ->
            listGetAt index meshes
                |> Maybe.map (MeshNode matrix)

        GLTF.CameraNode matrix index ->
            listGetAt index cameras
                |> Maybe.map (CameraNode matrix)

        GLTF.Group matrix children ->
            let
                childNodes =
                    List.filterMap
                        (\( index, node ) ->
                            if List.member index children then
                                buildTreeHelp meshes cameras allNodes node

                            else
                                Nothing
                        )
                        allNodes
            in
            Just (Group matrix childNodes)
