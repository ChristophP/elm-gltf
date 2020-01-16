# Elm GLTF

A package for loading and working with 3D models in the [GLTF](https://github.com/KhronosGroup/glTF/tree/master/specification/2.0) format.

Link to GLTF cheat sheet [here](https://www.khronos.org/files/gltf20-reference-guide.pdf).

## Current Results

Idea: Create 3 levels

Low-Level: Raw GLTF (normalized, looks like GLTF, Buffers unparsed)
Mid-Level: Tree of things(denormalized, buffers parsed)
High-Level: Just a list of Drawables, and a list of cameras.

Things that can get expensive when done a lot:
- parsing buffers
- resolving relationsships:
  - accessors
  - nodes -> meshes
  - material -> textures

### 2020-01-12 Displaying some geometry
![First Result](/result_01.png?raw=true "First Result")
![Second Result](/result_02.png?raw=true "Second Result")
