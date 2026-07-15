# harfarasta/export

`harfarasta/export` provides a single high-level entry point,
`render-string`, for rendering text directly to a PNG image or a Wavefront
OBJ mesh file -- without touching the lower-level shaping/rendering API
documented in [`docs/shaping.md`](shaping.md), [`docs/rendering.md`](rendering.md),
and [`docs/triangulation.md`](triangulation.md) yourself.

Package: `harfarasta/export` (nickname `rich-text/export`).

## Loading

```lisp
(ql:quickload :harfarasta/export)
```

Depends on `harfarasta` and `zpng`.

## `render-string`

```lisp
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

### Parameters

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
| `:png-size` | `nil` | `'(W H)` for a fixed canvas size, or `nil` for auto-fit (PNG only) |
| `:anti-alias` | `t` | When `nil`, PNG uses fast direct rasterization instead of SDF+smoothstep, and OBJ uses ear-clipping (`cl-earcut`) instead of constrained Delaunay for triangulation |

## PNG output

Built on the anti-aliased bitmap path (`shape-to-bitmap`, see
[`docs/rendering.md`](rendering.md)) by default, or the fast binary path when
`:anti-alias nil`. Pixels come from `bitmap-to-bytes` and are written via
`zpng` with a transparent background (alpha channel derived from coverage).

## OBJ output

Built on `text-to-meshes`/`text-to-meshes-fast` (see
[`docs/triangulation.md`](triangulation.md)), selected by `:anti-alias`. The
writer:

- Computes `scale = size / units-per-em`.
- Converts `:depth` from output units back to font units (`depth-fu = depth /
  scale`) so triangulation happens in font-unit space and the final `* scale`
  produces the requested output-unit depth.
- Writes one merged OBJ file: each glyph's vertices are scaled, offset by its
  pen position, and Y-flipped from font-unit Y-down to OBJ's Y-up; each
  glyph's triangle indices are remapped with a running vertex offset so every
  glyph shares one vertex namespace across `v`/`f` lines.

This scale/offset/remap logic is also what `harfarasta/mesh` mirrors to
produce `common-shapes:mesh` objects instead of an OBJ file -- see
[`docs/mesh.md`](mesh.md).

## Tests

`harfarasta/tests` exercises `render-string` (both PNG and OBJ, anti-aliased
and fast paths, word wrap, fixed canvas sizes) via
`harfarasta/tests:render-tests`.
