# harfarasta/mesh

`harfarasta/mesh` converts glyph and text geometry produced by the core
`harfarasta` library into [`common-shapes:mesh`](https://git.sr.ht/~takeiteasy/common-shapes)
objects, so glyph meshes compose with `common-shapes`' generators, CSG
operations, and matrix/serialization utilities.

Package: `harfarasta/mesh` (nickname `rich-text/mesh`).

## Loading

```lisp
(ql:quickload :harfarasta/mesh)
```

Depends on `harfarasta` and `common-shapes`.

## API

### `glyph-mesh (font glyph-id &key (size 64) depth fast normals (segments-per-edge 8))`

Triangulate a single glyph into a `common-shapes:mesh`. Returns `NIL` for
blank glyphs (e.g. space).

- `size` -- target coordinate scale. Font-unit coordinates are divided by the
  font's units-per-em and multiplied by `size` (same convention as
  `harfarasta/export`'s `render-string :as :obj`).
- `depth` -- when non-`NIL`, extrudes the glyph along Z by `depth` units and
  the resulting mesh has `dimensions` 3 instead of 2.
- `fast` -- when true, triangulates via ear-clipping (`cl-earcut`) instead of
  constrained Delaunay (`cl-constrained-delaunay`, the default).
- `normals` -- when true, fills `+Z` normals per vertex, but **only for flat
  (non-extruded) meshes**. Extruded (`depth` non-`NIL`) meshes never get
  normals from this package -- see "Normals" below.
- `segments-per-edge` -- curve tessellation quality, passed through to the
  underlying `shape-to-mesh`/`shape-to-mesh-fast`.

### `text-meshes (font text &key (size 64) depth fast normals ...)`

Shape `text` with `font` and triangulate each visible glyph into a positioned
`common-shapes:mesh`. Returns a list of meshes, one per rendered glyph, each
already translated to its pen position -- so glyph identity and order are
preserved for per-letter transforms or animation.

Accepts the same `size`/`depth`/`fast`/`normals`/`segments-per-edge` keys as
`glyph-mesh`, plus shaping/layout keys passed through to
`rich-text:text-to-meshes` / `rich-text:text-to-meshes-fast`: `direction`,
`script`, `language`, `alignment`, `line-height`, `max-width`, `wrap`,
`fallback-fonts`, `basic`.

### `text-mesh (font text &key (size 64) depth fast normals ...)`

Same shaping/layout as `text-meshes`, but returns a single merged
`common-shapes:mesh` with all glyphs sharing one vertex/index namespace
(indices remapped with a running vertex offset, mirroring
`harfarasta/export`'s OBJ writer). Returns a valid but empty mesh (zero
vertices/indices, correct `dimensions`) if `text` has no visible glyphs.

## Coordinate conventions

Mesh coordinates match `harfarasta/export`'s OBJ output exactly:

- `scale = size / units-per-em`.
- Each glyph's pen offset (in font units) is baked into its vertices before
  scaling.
- Y is flipped from font-unit Y-down to +Y-up, matching `common-shapes`'
  own convention.
- Triangle winding is counter-clockwise, as `common-shapes`' `triangle-normal`
  and `compute-normals` expect.
- The mesh sits with its baseline at the origin and is roughly `size` units
  tall (not centered at the origin, unlike `common-shapes`' own shape
  generators).

## Normals

`common-shapes:compute-normals` errors on 2D meshes and, for extruded (3D)
meshes, averages normals across adjacent faces -- which would smooth across
the hard seams between a glyph's front face, back face, and extrusion side
walls. To avoid surprising results:

- Flat (2D) meshes: `normals` is `NIL` by default; pass `:normals t` to get a
  flat `+Z` normal per vertex.
- Extruded (3D, `depth` non-`NIL`) meshes: `normals` is always `NIL` here.
  Callers who want normals on an extruded mesh should compute them explicitly
  (e.g. via `common-shapes:compute-normals`, understanding it will smooth
  across seams) rather than relying on this package to guess the right
  policy.

## Example

```lisp
(rich-text:with-font (font "/path/to/font.ttf")
  ;; A single glyph
  (let* ((glyph-id (rich-text:shaped-glyph-glyph-id
                     (first (rich-text:shape-text font "A"))))
         (mesh (rich-text/mesh:glyph-mesh font glyph-id :size 64)))
    (format t "verts=~D tris=~D~%"
            (common-shapes:vertex-count mesh)
            (common-shapes:triangle-count mesh)))

  ;; Per-glyph meshes for "Hi", positioned along the baseline
  (dolist (mesh (rich-text/mesh:text-meshes font "Hi" :size 64))
    (format t "verts=~D~%" (common-shapes:vertex-count mesh)))

  ;; One merged, extruded mesh for the whole string
  (let ((mesh (rich-text/mesh:text-mesh font "Hi" :size 64 :depth 0.1)))
    (format t "merged verts=~D tris=~D dims=~D~%"
            (common-shapes:vertex-count mesh)
            (common-shapes:triangle-count mesh)
            (common-shapes:mesh-dimensions mesh))))
```

## Tests

`harfarasta/tests` exercises this package via
`harfarasta/tests:mesh-tests`, which builds glyph and text meshes and asserts
array element types, dimensions, vertex/triangle counts, and counter-clockwise
winding.
