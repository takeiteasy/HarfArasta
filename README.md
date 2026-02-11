# HarfArasta 

HarfArasta (آراسته) is a platform/backend-agnostic text rendering and shaping library for Common Lisp that uses [HarfBuzz](https://harfbuzz.github.io/) for text shaping. After shaping, it renders glyphs as SDF, MSDF, anti-aliased bitmap, or triangulated mesh -- suitable for GPU text rendering, game engines, UI toolkits, or offline export.

## Features

- **HarfBuzz text shaping** -- full OpenType layout (ligatures, kerning, BiDi, script/language support)
- **Font discovery** -- find system fonts by family, weight, slant, spacing, or stretch
- **Glyph outline extraction** -- convert glyph outlines to vector shapes
- **SDF rendering** -- single-channel signed distance fields
- **MSDF rendering** -- multi-channel signed distance fields for sharp corners at any zoom
- **Bitmap rendering** -- anti-aliased grayscale coverage bitmaps via SDF thresholding
- **Mesh generation** -- constrained Delaunay triangulation of glyph outlines
- **String rendering** -- shape a string and render all visible glyphs with correct positioning
- **PNG/OBJ export** -- render strings to PNG images or Wavefront OBJ meshes (via `harfarasta/export`)

## Quickstart

```lisp
;; Load the library
(ql:quickload :harfarasta)

;; Open a font by path
(rich-text:with-font (font "/path/to/font.ttf")
  ;; Shape a string
  (let ((glyphs (rich-text:shape-text font "Hello, World!")))
    (dolist (g glyphs)
      (format t "glyph=~D advance=~D~%"
              (rich-text:shaped-glyph-glyph-id g)
              (rich-text:shaped-glyph-x-advance g))))

  ;; Render each glyph as an SDF bitmap
  (let ((sdfs (rich-text:text-to-sdfs font "Hello" 64 64)))
    (dolist (entry sdfs)
      (format t "x=~D y=~D bitmap=~A~%" (first entry) (second entry) (third entry))))

  ;; Render as anti-aliased bitmaps
  (let ((bitmaps (rich-text:text-to-bitmaps font "Hello" 64 64)))
    (dolist (entry bitmaps)
      (format t "x=~D bitmap-w=~D~%"
              (first entry)
              (trivial-sdf:bitmap-width (third entry)))))

  ;; Generate triangle meshes
  (let ((meshes (rich-text:text-to-meshes font "Hi")))
    (dolist (entry meshes)
      (format t "x=~D verts=~D tris=~D~%"
              (first entry)
              (/ (length (third entry)) 2)
              (/ (length (fourth entry)) 3)))))

;; Or discover a font by family name
(rich-text:with-font (font :family "Helvetica" :weight :bold)
  (rich-text:shape-text font "Bold text"))
```

## Building

### HarfBuzz shared library

```sh
mkdir -p build && cd build && cmake .. && make
```

### Loading

```lisp
(ql:quickload :harfarasta)
```

### Running tests

```lisp
(asdf:test-system :harfarasta)
```

## Systems

| System | Description |
|--------|-------------|
| `harfarasta` | Core library (shaping, SDF/MSDF/bitmap/mesh rendering, font discovery) |
| `harfarasta/harfbuzz` | Internal CFFI bindings to HarfBuzz (not part of public API) |
| `harfarasta/export` | PNG and OBJ export utilities |
| `harfarasta/tests` | Test suite (fiveam) |

## Dependencies

- [cffi](https://github.com/cffi/cffi) -- foreign function interface for HarfBuzz bindings
- [trivial-signed-distance-fields](https://github.com/takeiteasy/trivial-signed-distance-fields) -- SDF/MSDF glyph rendering
- [trivial-delaunay-triangulation](https://github.com/takeiteasy/trivial-delaunay-triangulation) -- CDT mesh generation
- [font-discovery](https://shinmera.com/project/font-discovery) -- system font lookup
- [harfbuzz](https://harfbuzz.github.io/) -- text shaping engine (C shared library, built via CMake)
- [zpng](https://www.xach.com/lisp/zpng/) -- PNG export (`harfarasta/export` only)
- [fiveam](https://github.com/lispci/fiveam) -- testing (`harfarasta/tests` only)

## Export Package

`harfarasta/export` provides a simple interface to render strings directly to PNG images or Wavefront OBJ mesh files.

```lisp
(ql:quickload :harfarasta/export)

;; Render to PNG (anti-aliased, transparent background)
(rich-text/export:render-string "Hello" #p"hello.png"
  :as :png :family "Arial" :size 128 :color '(255 255 255))

;; Render to OBJ (triangulated mesh)
(rich-text/export:render-string "Hello" #p"hello.obj"
  :as :obj :family "Helvetica" :size 1.0)

;; Use a specific font file instead of discovery
(rich-text/export:render-string "Test" #p"test.png"
  :as :png :font-path "/path/to/font.ttf" :size 96 :color '(255 0 0))

;; Run the built-in render tests (writes to export-tests/)
(rich-text/export:render-tests)
```

### `render-string` parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `text` | *(required)* | String to render |
| `file` | *(required)* | Output pathname |
| `:as` | `:png` | Output format: `:png` or `:obj` |
| `:font-path` | `nil` | Path to a TTF/OTF file (overrides discovery) |
| `:family` | `"Arial"` | Font family for discovery |
| `:weight` | `:regular` | Font weight for discovery |
| `:size` | `64` | Pixel height (PNG) or unit scale (OBJ) |
| `:color` | `(255 255 255)` | RGB color list, 0-255 (PNG only) |

## License

[GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)
