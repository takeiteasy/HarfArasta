# Glyph rendering: SDF, MSDF, and bitmaps

This covers everything in core `harfarasta` that turns a glyph outline into a
*raster* (a `bitmap`) rather than a triangle mesh. For mesh generation, see
[`docs/triangulation.md`](triangulation.md); for converting meshes into
`common-shapes:mesh` objects, see [`docs/mesh.md`](mesh.md).

## Which one do I want?

- **SDF** (single-channel signed distance field) -- the general-purpose choice.
  Cheap to generate and to sample, and because it stores *distance to the
  outline* rather than raw coverage, a single bitmap can be resampled at any
  size with a shader-side smoothstep -- the classic use case is GPU text
  rendering, where one small SDF atlas glyph is scaled up in a fragment shader
  with sharp edges preserved. `shape-to-bitmap` (the anti-aliased raster path)
  is itself built on top of SDF generation.
- **MSDF** (multi-channel signed distance field) -- like SDF but preserves
  sharp corners at small sizes. Regular SDF rounds off corners when magnified
  a lot (e.g. a small atlas glyph blown up for a large heading); MSDF encodes
  distance in three color channels with per-edge coloring so a median-of-3
  reconstruction recovers sharp corners. Costs more to generate (edge coloring
  + a more expensive per-pixel computation) -- reach for it only when corner
  sharpness at scale actually matters.
- **Bitmap** -- a plain grayscale (or binary) raster, no distance-field math
  exposed to the caller. Use this for direct rendering (previews, fixed-size
  icons, non-GPU rasterization) where you just want pixels, not a resamplable
  distance field. Comes in an anti-aliased flavor (`shape-to-bitmap`, built on
  SDF + smoothstep) and a fast binary flavor (`shape-to-bitmap-fast`, direct
  winding-number rasterization, no distance field at all). `render-string`'s
  PNG output (`harfarasta/export`) is built on this anti-aliased path, with
  `:anti-alias nil` switching to the fast path.

Every function here comes in the usual three tiers: `shape-to-*` (operates on
a `shape` from `glyph-to-shape`), `glyph-to-*` (font + glyph-id, returns `NIL`
for blank glyphs), and `text-to-*` (shapes a whole string, returns a
positioned list per visible glyph -- multi-line, alignment, and word-wrap all
supported the same way as `text-to-meshes`).

## SDF

```lisp
(shape-to-sdf shape width height &key (range 4.0d0) (padding 2.0))
(glyph-to-sdf font glyph-id width height &key (range 4.0d0) (padding 2.0))
(text-to-sdfs font text glyph-width glyph-height
              &key direction script language (range 4.0d0) (padding 2.0)
                   alignment line-height max-width (wrap :word)
                   (fallback-fonts *fallback-fonts*) basic)
```

- `width`/`height` -- output bitmap dimensions in pixels.
- `range` -- the distance-field range, in *shape units*, that maps to the
  `[0, 1]` output range. Larger values compress more of the outline's
  surrounding distance field into the bitmap (softer/wider falloff); smaller
  values sharpen it.
- `padding` -- border padding in pixels, handled by `auto-scale-shape`
  fitting the glyph into the canvas (see below).
- `text-to-sdfs` returns a list of `(x y bitmap)` per glyph, matching the
  `(x y . data)` shape used throughout the shaping API.

Output convention: a 1-channel `bitmap` where **`< 0.5` = inside** the glyph,
`0.5` = exactly on the edge, `> 0.5` = outside.

`generate-sdf-from-shape` is the low-level primitive underneath `shape-to-sdf`
-- it takes an explicit `scale`/`translate-x`/`translate-y` instead of
auto-fitting to a canvas via `auto-scale-shape`. Reach for it directly when
you need precise control over the shape-to-pixel mapping (e.g. packing many
glyphs into a shared atlas at a font-wide scale, rather than each glyph
auto-scaled independently to fill its own bitmap).

## MSDF

```lisp
(shape-to-msdf shape width height &key (range 4.0d0) (padding 2.0))
(glyph-to-msdf font glyph-id width height &key (range 4.0d0) (padding 2.0))
(text-to-msdfs font text glyph-width glyph-height
               &key direction script language (range 4.0d0) (padding 2.0)
                    alignment line-height max-width (wrap :word)
                    (fallback-fonts *fallback-fonts*) basic)
```

Same parameters and auto-scaling as SDF, but returns a 3-channel `bitmap`
(RGB) instead of 1-channel. Before generating, the shape's edges are colored
(`edge-coloring.lisp`) so that each outline edge is assigned to one or two of
the three channels, alternating at sharp corners. Reconstructing the outline
as `median(R, G, B)` recovers hard corners that a plain single-channel SDF
would round off when the glyph is scaled up a lot -- this is the entire
reason MSDF exists over SDF. The tradeoff is the extra edge-coloring pass and
a pixel-clash correction step (`msdf.lisp`) that detects and repairs
channel-assignment artifacts near corners.

## Bitmap rendering

```lisp
;; Anti-aliased (SDF-based)
(shape-to-bitmap shape width height &key (range 4.0d0) (padding 2.0) (edge-width nil))
(glyph-to-bitmap font glyph-id width height &key (range 4.0d0) (padding 2.0) (edge-width nil))
(text-to-bitmaps font text glyph-width glyph-height
                 &key direction script language (range 4.0d0) (padding 2.0) (edge-width nil)
                      alignment line-height max-width (wrap :word)
                      (fallback-fonts *fallback-fonts*) basic)

;; Fast (binary, no AA)
(shape-to-bitmap-fast shape width height &key (padding 2.0) scale tx ty)
(glyph-to-bitmap-fast font glyph-id width height &key (padding 2.0))
(text-to-bitmaps-fast font text glyph-width glyph-height
                      &key direction script language (padding 2.0)
                           alignment line-height max-width (wrap :word)
                           (fallback-fonts *fallback-fonts*) basic)
```

`shape-to-bitmap` generates an SDF internally, then applies smoothstep
thresholding around the `0.5` edge to produce a 1-channel anti-aliased
coverage bitmap (`1.0` = inside, `0.0` = outside -- note this is the inverse
of the SDF's own `< 0.5` = inside convention). `edge-width` controls how wide
the anti-aliasing transition is; `NIL` auto-computes roughly one pixel of AA
based on bitmap size.

`shape-to-bitmap-fast` skips distance fields entirely: it rasterizes
directly via winding-number tests per pixel (the same `%shape-winding-at`
used by fast SDF sign resolution), producing a purely binary bitmap (`1.0`/
`0.0`, no anti-aliasing). Much cheaper, at the cost of jagged edges -- pass
`scale`/`tx`/`ty` explicitly to skip the auto-fit step when rendering many
glyphs at a shared scale (e.g. atlas packing), the same escape hatch
`generate-sdf-from-shape` offers for SDF.

### The `bitmap` struct

```lisp
(bitmap-data bmp)     ; (simple-array single-float (*)), row-major, interleaved channels
(bitmap-width bmp)    ; fixnum
(bitmap-height bmp)   ; fixnum
(bitmap-channels bmp) ; 1 (grayscale/SDF) or 3 (RGB/MSDF)
(bitmap-to-bytes bmp) ; => (simple-array (unsigned-byte 8) (*)), clamped [0,1] -> [0,255]
```

`bitmap-to-bytes` is the usual last step before handing pixels to a PNG
encoder or texture upload -- `harfarasta/export`'s PNG output does exactly
this.

## Shape utilities

```lisp
(shape-bounds shape)                          ; => (values min-x min-y max-x max-y)
(auto-scale-shape shape width height &key (padding 0.0))
                                               ; => (values scale translate-x translate-y)
```

`shape-bounds` walks every edge's control points to compute a glyph's
bounding box in font units. `auto-scale-shape` uses that bounding box to
compute the scale and translation that fits the glyph into a `width`x`height`
canvas with `padding` pixels of border, preserving aspect ratio (the smaller
of the x/y scale factors wins) and centering the glyph. This is what
`shape-to-sdf`/`shape-to-msdf`/`shape-to-bitmap`/`shape-to-bitmap-fast` all
call internally when no explicit scale is given -- use it directly if you're
building a custom rendering pipeline that needs the scale/translate numbers
before generating a bitmap.

## Example

```lisp
(ql:quickload :harfarasta)

(rich-text:with-font (font "/path/to/font.ttf")
  ;; Single glyph as an SDF, for a GPU-side atlas
  (let* ((shape (rich-text:glyph-to-shape
                 font (rich-text:shaped-glyph-glyph-id
                       (first (rich-text:shape-text font "A")))))
         (sdf (rich-text:shape-to-sdf shape 64 64 :range 4.0d0)))
    (format t "sdf: ~Dx~D, ~D channel(s)~%"
            (rich-text:bitmap-width sdf)
            (rich-text:bitmap-height sdf)
            (rich-text:bitmap-channels sdf)))

  ;; Whole string as anti-aliased bitmaps, positioned along the baseline
  (dolist (entry (rich-text:text-to-bitmaps font "Hi" 64 64))
    (destructuring-bind (x y bitmap) entry
      (format t "x=~D y=~D bytes=~D~%"
              x y (length (rich-text:bitmap-to-bytes bitmap))))))
```
