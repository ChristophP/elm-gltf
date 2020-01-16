module Scene exposing (..)

import GLTF
import Math.Matrix4 as Mat4
import Mesh
import Set
import Util
import WebGL


type Node
    = MeshNode Mat4.Mat4 (WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes)
    | CameraNode Mat4.Mat4 GLTF.Camera
    | Group Mat4.Mat4 (List Node)


type alias Scene =
    List Node


type alias Drawable =
    { mesh : WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes }


type alias Camera =
    GLTF.Camera


getDrawables : GLTF.GLTF -> List Drawable
getDrawables gltf =
    []


getCameras : GLTF.GLTF -> List ( Mat4.Mat4, Camera )
getCameras gltf =
    []



-- Nodes


getRootNodes : List ( Int, GLTF.Node ) -> List GLTF.Node
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


buildTreeFromRootNodes : List GLTF.Node -> List Node
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


buildTreeFromRootNodesHelp : List ( Int, GLTF.Node ) -> GLTF.Node -> Node
buildTreeFromRootNodesHelp allNodes rootNode =
    case rootNode of
        GLTF.MeshNode matrix index ->
            listGetAt index allNodes
                |> Maybe.map
                    (\node ->
                        -- TODO find actual Mesh)
                        Group []
                    )

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
            Group rootNode.matrix childNodes
