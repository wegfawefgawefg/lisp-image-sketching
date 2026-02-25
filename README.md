# lispfiddling

Common Lisp experiments, including PNG image generation.

## Project layout

- `src/main.lisp`: CLI entry and scene dispatch
- `src/render-core.lisp`: shared rendering/image helpers
- `src/scene-registry.lisp`: scene registration + lookup
- `src/scenes/`: auto-discovered scene files
- `scripts/`: thin wrappers (`generate.lisp`, `build.lisp`)
- `outputs/`: generated image files (`.png`)
- `assets/`: README preview images
- `build/`: compiled artifacts (`.fasl`)

## Image previews

### Sunsphere (480x270 preview)

![Sunsphere preview](assets/sunsphere_480x270.png)

### Sine Stack Classic (480x270 preview)

![Sine Stack Classic preview](assets/sine_stack_classic_480x270.png)

## Run and build

- List scenes:
  - `make run IMAGE=list`
- Generate one scene:
  - `make run IMAGE=sine DOWNSAMPLE=3`
  - first arg is scene, second arg is downsample factor in Lisp CLI
- Generate all scenes:
  - `make run` (defaults to `IMAGE=all`)
- Compile source:
  - `make build`
- Clean generated files:
  - `make clean`

Scenes are auto-discovered from `src/scenes/*.lisp`, so adding a new scene file
and calling `register-scene` is enough to make it available.

The renderer uses Quicklisp + `zpng` for direct PNG output.

## Timeline

Based on file last-modified timestamps (local time, UTC+09:00):

- **2023-02-05**
  - `src/main.lisp` (Lisp/Clack + language experiments)
  - `potato.data` (basic file I/O test, values 0-9)
  - `outputs/example.ppm` (early image output)
- **2023-02-07**
  - `src/old.lisp` (older vec3 + file/read-write scratch code)
  - `outputs/test.ppm` and `outputs/some_boxes.ppm` (PPM image outputs)
- **2023-02-12**
  - `src/makepic.lisp` (main image-generation work: vec3 pixels, PPM writer, rect drawing)
