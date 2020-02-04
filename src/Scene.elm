module Scene exposing (Scene, fromGLTF, getCameras, getDrawables)

import GLTF
import Math.Matrix4 as Mat4
import Mesh
import Set
import Util exposing (listGetAt, maybeSequence)
import WebGL
import WebGL.Texture as Texture


type Node
    = MeshNode Mat4.Mat4 (WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes) Texture.Texture
    | CameraNode Mat4.Mat4 GLTF.Camera
    | Group Mat4.Mat4 (List Node)


fromGLTF : GLTF.GLTF -> Maybe Scene
fromGLTF gltf =
    let
        maybeTextureTasks =
            GLTF.resolveMaterials gltf

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


{-| Gets a list of Entities from the scene which you can directly render in
your app. It uses default shaders. If you need more control use
[`getDrawables`](#getDrawables)
-}
getEntities : Scene -> List WebGL.Entity
getEntities scene =
    Debug.todo "Implement me"


{-| The Drawable is a temporary constuct to get things to work out in the example
for now. It may be more sensible to make it more sophisticated or go straight
to a WebGL.Entity instead.
TODO: This should contain a lot more than what is currently supported, such as
texture options, sampler data, PBR values etc.
-}
type alias Drawable =
    { mesh : WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes
    , worldTransform : Mat4.Mat4
    , texture : Texture.Texture
    }


{-| Gets drawable items and their associated data from the scene. Use this
when you still want to customize how rendering works such as supplying your own
shaders, or tweaking texture options
-}
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

                MeshNode matrix mesh texture ->
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

                MeshNode _ _ _ ->
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



-- buildMeshNode : Mesh -> Material -> PBRMR -> TextureInfo -> Sampler & Image -> Texture
--buildMesh : GLTF -> Mesh.Mesh -> (WegGL.Mesh Mesh.PositionNormalTexCoordsAttributes, Texture Texture)
--buildMesh gltf
