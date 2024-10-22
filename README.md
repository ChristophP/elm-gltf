# Elm GLTF

## UNFINISHED! Working on it took very long so I stopped. I'm putting it here so others can take a look at it and potentially fork.

A package for loading and working with 3D models in the [GLTF](https://github.com/KhronosGroup/glTF/tree/master/specification/2.0) format.

Link to GLTF cheat sheet [here](https://www.khronos.org/files/gltf20-reference-guide.pdf).

## Run the example

```sh
npm install
cd examples
npx elm reactor # compile elm code and spin up a dev server
```

## Features

The glTF spec is quite large, especially when also considering extensions.
The following gives a quick overview of feature that `elm-gltf` aims to
support in the foreseeable future.

### Data extraction
- [x] Extracting scenes
- [x] Extracting nodes
- [x] Extracting buffers
- [x] Extracting buffer views
- [x] Extracting accessors
- [x] Extracting meshes
- [x] Extracting textures
- [x] Extracting texture properties
- [ ] Extracting TRS properties (separate rotation, scale and translation)
- [ ] Extracting meshes with different vertex attributes (right now only position, normal, texCoords)
- [ ] Extracting different materials (PBR, Lambertian, etc)
- [ ] Extracting different mesh types (lines, etc)
- [ ] Extracting orthographic and infinite perspective projection

### Formats (Where are the assets?)
- [x] Embedded glTF(all assets like buffers and images are embedded as base64, single file)
- [ ] Hyperlinked glTF(assets are referenced via URL, multiple files)
- [ ] Binary GLB (everything is bundled into a single binary container file)
- [ ] possibly extensions like DRACO

## Current Results

Idea: Create 3 levels

1. Low-Level: Raw GLTF (normalized, looks like GLTF, Buffers unparsed)
2. Mid-Level: Tree of things(denormalized, buffers parsed)
3. High-Level: Just a list of Drawables, and a list of cameras.

Things that can get expensive when done a lot:
- parsing buffers
- resolving relationsships:
  - accessors
  - nodes -> meshes
  - material -> textures

`Scene` could be just something simple to draw. If something like modifying
the scene data is needed, there could be an integration with `elm-3d-scene` for
that.

### 2020-01-21 Using a Camera contained in the glTF
![Third Result](/result_03.png?raw=true "Third Result")
### 2020-01-17 Showing some (hardcoded) texture
![Second Result](/result_02.png?raw=true "Second Result")
### 2020-01-12 Displaying some geometry
![First Result](/result_01.png?raw=true "First Result")
