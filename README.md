# HarfArasta 

HarfArasta (آراسته) is a platform/backend-agnostic text rendering and shaping library for Common Lisp that uses [HarfBuzz](https://harfbuzz.github.io/) for text shaping. After shaping, it renders glyphs as SDF, MSDF, bitmap, or triangulated mesh -- suitable for GPU text rendering, game engines, UI toolkits, or offline export.

## Features

- **HarfBuzz text shaping** -- full OpenType layout (ligatures, kerning, BiDi, script/language support)
- **Glyph outline extraction** -- convert glyph outlines to vector shapes
- **M/SDF rendering** -- multi-channel and single-channel signed distance fields
- **Bitmap rendering** -- anti-aliased grayscale coverage bitmaps via SDF thresholding by default, fast direct rendering optionally
- **Mesh generation** -- constrained Delaunay triangulation of glyph outlines
- **PNG/OBJ export** -- render strings to PNG images or Wavefront OBJ meshes (via `harfarasta/export`)
- **Extra shaping** -- automatic line breaking at a configurable max width (word or glyph boundary modes), newline support and alignment.
- **WOFF1/WOFF2** -- Web Open Font Format 1.0/2.0

### TODO

- [ ] Add svg output for `harfarasta/export`
- [ ] Font atlas export for `harfarasta/export`
- [ ] Option to disable text shaping (harfbuzz)

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
              (bitmap-width (third entry)))))

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
| `harfarasta/harfbuzz` | Internal CFFI bindings to HarfBuzz |
| `harfarasta/woff2` | Internal CFFI bindings to libwoff2shim for WOFF2 decoding |
| `harfarasta/export` | PNG and OBJ export utilities |
| `harfarasta/tests` | Tests using `harfarasta/export` |

## Dependencies

- [cffi](https://github.com/cffi/cffi) -- foreign function interface for HarfBuzz bindings
- [harfbuzz](https://github.com/harfbuzz/harfbuzz) -- text shaping engine (C shared library, built via CMake)
- [woff2](https://github.com/google/woff2) -- WOFF2 decoder (C shared library, built via CMake)
- [font-discovery](https://shinmera.com/project/font-discovery) -- system font lookup
- [zpng](https://www.xach.com/lisp/zpng/) -- PNG export (`harfarasta/export` only)

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
| `:depth` | `nil` | Z extrusion depth in output units (OBJ only) |
| `:alignment` | `:left` | `:left`, `:center`, or `:right` |
| `:line-height` | `nil` | Y distance between lines in font units (default = upem) |
| `:fallback-fonts` | `nil` | Font pointers tried for missing glyphs |
| `:max-width` | `nil` | Max text width — pixels for PNG, output units for OBJ; triggers word wrapping |
| `:wrap` | `:word` | `:word` — break at word boundaries (default); `:glyph` — break at any glyph |
| `:png-size` | `nil` | `'(W H)` for a fixed canvas size, or `nil` for auto-fit (PNG only) 
| `:anti-alias` | `t` | When `nil`, renderer will skip smoothstep and SDF steps |

## License

[GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)
