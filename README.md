# HarfArasta 

HarfArasta (آراسته) is a platform/backend-agnostic text rendering and shaping library for Common Lisp that uses [HarfBuzz](https://harfbuzz.github.io/) for text shaping. After shaping, it renders glyphs as SDF, MSDF, bitmap, or triangulated mesh -- suitable for GPU text rendering, game engines, UI toolkits, or offline export.

## Features

- **HarfBuzz text shaping** -- full OpenType layout (ligatures, kerning, BiDi, script/language support) (optional `:basic`)
- **Glyph outline extraction** -- convert glyph outlines to vector shapes
- **M/SDF rendering** -- multi-channel and single-channel signed distance fields
- **Bitmap rendering** -- anti-aliased grayscale coverage bitmaps via SDF thresholding by default, fast direct rendering optionally (`:anti-alias` or `:fast`)
- **Mesh generation** -- constrained Delaunay triangulation of glyph outlines (via `cl-constrained-delaunay`) or ear-clipping (`:fast`, via `cl-earcut`)
- **PNG/OBJ export** -- render strings to PNG images or Wavefront OBJ meshes (via `harfarasta/export`)
- **common-shapes integration** -- glyph and text meshes as `common-shapes:mesh` objects for composing with `common-shapes` generators, CSG, and transforms (via `harfarasta/mesh`)
- **Extra shaping** -- automatic line breaking at a configurable max width (word or glyph boundary modes), newline support and alignment.
- **WOFF1/WOFF2** -- Web Open Font Format 1.0/2.0

### TODO

- [ ] Add svg output for `harfarasta/export`
- [ ] Font atlas export for `harfarasta/export`

## Documentation

| Doc | Covers |
|-----|--------|
| [`docs/shaping.md`](docs/shaping.md) | Font loading/lifecycle, HarfBuzz shaping, word wrap, font inspection, fallback fonts, WOFF1/WOFF2 |
| [`docs/rendering.md`](docs/rendering.md) | SDF, MSDF, and bitmap glyph rendering |
| [`docs/triangulation.md`](docs/triangulation.md) | Core glyph-to-mesh triangulation (constrained Delaunay / earcut) |
| [`docs/mesh.md`](docs/mesh.md) | `harfarasta/mesh` -- `common-shapes:mesh` integration |
| [`docs/export.md`](docs/export.md) | `harfarasta/export` -- `render-string` for PNG/OBJ output |
| [`docs/fontstash.md`](docs/fontstash.md) | `harfarasta/fontstash` -- texture atlas / glyph packing |

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
| `harfarasta/mesh` | `common-shapes:mesh` integration for glyph/text meshes |
| `harfarasta/fontstash` | Font atlas / glyph packing for GPU text rendering |
| `harfarasta/tests` | Tests using `harfarasta/export`, `harfarasta/mesh`, and `harfarasta/fontstash` |

## Dependencies

- [cffi](https://github.com/cffi/cffi) -- foreign function interface for HarfBuzz bindings
- [harfbuzz](https://github.com/harfbuzz/harfbuzz) -- text shaping engine (C shared library, built via CMake)
- [woff2](https://github.com/google/woff2) -- WOFF2 decoder (C shared library, built via CMake)
- [font-discovery](https://shinmera.com/project/font-discovery) -- system font lookup
- [cl-earcut](https://git.sr.ht/~takeiteasy/cl-earcut) -- ear-clipping triangulation (`:fast` mesh generation)
- [cl-constrained-delaunay](https://git.sr.ht/~takeiteasy/cl-constrained-delaunay) -- constrained Delaunay triangulation (default mesh generation)
- [common-shapes](https://git.sr.ht/~takeiteasy/common-shapes) -- mesh data structure & shape/CSG interop (`harfarasta/mesh` only)
- [zpng](https://www.xach.com/lisp/zpng/) -- PNG export (`harfarasta/export` only)

## Export Package

`harfarasta/export` provides `render-string`, a single entry point to render
text directly to a PNG image or a Wavefront OBJ mesh file. See
[`docs/export.md`](docs/export.md) for the full parameter reference and
examples.

```lisp
(ql:quickload :harfarasta/export)
(rich-text/export:render-string "Hello" #p"hello.png"
  :as :png :family "Arial" :size 128 :color '(255 255 255))
```

## Mesh Package

`harfarasta/mesh` converts glyph and text geometry into
[`common-shapes:mesh`](https://git.sr.ht/~takeiteasy/common-shapes) objects, so
glyph meshes compose with `common-shapes`' generators, CSG operations, and
matrix transforms. See [`docs/mesh.md`](docs/mesh.md) for the full API and
conventions.

```lisp
(ql:quickload :harfarasta/mesh)
(rich-text:with-font (font "/path/to/font.ttf")
  (let ((mesh (rich-text/mesh:text-mesh font "Hi" :size 64)))
    (format t "verts=~D tris=~D~%"
            (common-shapes:vertex-count mesh)
            (common-shapes:triangle-count mesh))))
```

## License

[GPLv3](LICENSE)
