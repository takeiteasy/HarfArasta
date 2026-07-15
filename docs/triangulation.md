# Mesh generation (triangulation)

This document covers harfarasta's core glyph-to-triangle-mesh generation --
`shape-to-mesh`, `shape-to-mesh-fast`, and their glyph/text-level wrappers, all
in the core `harfarasta` package. For converting these meshes into
`common-shapes:mesh` objects (for composing with `common-shapes`' generators,
CSG, and transforms), see [mesh.md](mesh.md) instead -- that's a separate,
optional subsystem (`harfarasta/mesh`) built on top of the functions
documented here.

## API

### `shape-to-mesh (shape &key (segments-per-edge 8) depth)`

Triangulate a glyph outline (`shape`, from `glyph-to-shape`) via **constrained
Delaunay triangulation** (`cl-constrained-delaunay`). This is the default,
higher-quality path: it produces well-shaped, uniform triangles and is robust
to arbitrary contour topology.

Returns `(values vertices indices)`:
- `vertices` -- a flat `(simple-array single-float (*))`, interleaved `x,y`
  pairs (stride 2), or `x,y,z` triples (stride 3) when `depth` is non-`NIL`.
- `indices` -- a flat `(simple-array (unsigned-byte 32) (*))` of triangle
  index triples (every 3 elements is one triangle, 0-based).

`segments-per-edge` controls how many line segments each curved edge
(quadratic/cubic) is sampled into before triangulation; higher values give
smoother curves at the cost of more triangles. `depth`, when a number,
extrudes the mesh along Z -- see "3D extrusion" below.

### `shape-to-mesh-fast (shape &key (segments-per-edge 8) depth)`

Same signature and return format as `shape-to-mesh`, but triangulates via
**ear-clipping** (`cl-earcut`) instead. Much faster, and produces correct
results for standard glyph outlines (outer contours with nested holes), but
less uniform/well-shaped triangles than CDT, and less robust to unusual or
self-intersecting contour topology.

Ear-clipping needs to know which contours are outer boundaries and which are
holes. This is auto-detected per-shape from the largest (by absolute area)
contour: if it's clockwise, the TrueType convention is assumed (CW = outer,
CCW = hole); if counter-clockwise, the PostScript/OpenType convention is
assumed (CCW = outer, CW = hole). Each hole is then matched to the outer
contour that contains it.

### `glyph-to-mesh (font glyph-id &key (segments-per-edge 8) depth)` / `glyph-to-mesh-fast (font glyph-id &key (segments-per-edge 8) depth)`

Convenience wrappers: extract `glyph-id`'s outline from `font` via
`glyph-to-shape` and triangulate it. Return `NIL` for blank glyphs (e.g.
space) instead of `(values NIL NIL)`.

### `text-to-meshes (font text &key ...)` / `text-to-meshes-fast (font text &key ...)`

Shape `text` with `font` (via `shape-text`) and triangulate every visible
glyph. Returns a list of `(pen-x pen-y vertices indices)` entries -- one per
rendered glyph, in shaping order, with blank/skip glyphs omitted. `pen-x` and
`pen-y` are the glyph's pen position in **font units, Y-down** (add these to
each vertex, scaled appropriately, to place the glyph in string-space; see
`harfarasta/export`'s OBJ writer or `harfarasta/mesh` for worked examples of
this offset math).

Both accept `direction`, `script`, `language`, `segments-per-edge`, `depth`,
`alignment`, `line-height`, `max-width`, `wrap`, `fallback-fonts`, and `basic`
-- the same shaping/layout keys as `shape-text`.

## 3D extrusion

When `depth` is a number, the mesh is extruded along Z instead of staying
flat:
- A **front face** at `z = 0`, using the original 2D triangulation and
  winding.
- A **back face** at `z = depth`, using the same triangles but reversed
  winding (so both faces point outward).
- **Side walls** connecting the two faces: two triangles per contour edge,
  stitching the front and back contour outlines together.

The vertex count doubles (front + back copies of every 2D vertex) and
`vertices` becomes stride-3 (`x,y,z`).

## Winding

Constrained Delaunay output follows the winding of the input contours as
inserted. Ear-clipping's output winding follows whichever convention was
auto-detected (see above) -- it does not normalize to a fixed winding across
different fonts/glyphs. If you need a guaranteed winding (e.g. for
`common-shapes`, which expects CCW), check it explicitly; `harfarasta/mesh`
does this as part of its own test suite.

## Choosing CDT vs. fast/earcut

- **`shape-to-mesh` (CDT, default)** -- use when triangle quality matters (better
  for downstream mesh processing, subdivision, or rendering where triangle
  shape affects results) or when a shape's topology is unusual.
- **`shape-to-mesh-fast` (earcut)** -- use when throughput matters (e.g.
  triangulating many glyphs/whole strings at once) and the input is a
  standard glyph outline (outer contours with simple nested holes). This is
  what `render-string :anti-alias nil` and `:fast t` mesh options select.

## Example

```lisp
(rich-text:with-font (font "/path/to/font.ttf")
  ;; A single glyph
  (let ((glyph-id (rich-text:shaped-glyph-glyph-id
                    (first (rich-text:shape-text font "A")))))
    (multiple-value-bind (vertices indices)
        (rich-text:glyph-to-mesh font glyph-id)
      (format t "verts=~D tris=~D~%"
              (/ (length vertices) 2)
              (/ (length indices) 3))))

  ;; A whole string, extruded
  (dolist (entry (rich-text:text-to-meshes font "Hi" :depth 0.1))
    (destructuring-bind (pen-x pen-y vertices indices) entry
      (format t "pen=(~D,~D) verts=~D tris=~D~%"
              pen-x pen-y
              (/ (length vertices) 3)   ; stride 3: depth was requested
              (/ (length indices) 3)))))
```
