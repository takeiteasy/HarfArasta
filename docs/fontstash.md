# harfarasta/fontstash

`harfarasta/fontstash` packs rendered glyphs into a texture atlas -- a single
bitmap containing many glyphs, plus UV rectangles for each, suitable for GPU
text rendering. Package: `harfarasta/fontstash` (nickname `rich-text/fontstash`).

## Loading

```lisp
(ql:quickload :harfarasta/fontstash)
```

Depends on `harfarasta` and `zpng`.

## Creating an atlas

### `make-font-atlas (&key (width 512) (height 512) (mode :sdf) (padding 1))`

Creates an empty `font-atlas`. `mode` selects how glyphs are rendered into
the atlas: `:sdf` (1-channel signed distance field, the default), `:msdf`
(3-channel multi-channel SDF), `:bitmap` (anti-aliased coverage), or
`:bitmap-fast` (direct rasterization, no anti-aliasing). Channel count is
derived from `mode` (3 for `:msdf`, 1 otherwise). `padding` is the pixel
border kept between packed glyphs.

## Packing glyphs

### `atlas-add-glyph (atlas font glyph-id w h)`

Render a single glyph at exactly `w` x `h` pixels and pack it. Returns an
`atlas-entry`, or `NIL` if the glyph is blank or doesn't fit. If `glyph-id`
is already packed, returns the existing entry instead of re-rendering.

### `atlas-add-glyphs (atlas font glyph-ids w h)`

Same as above for a list of glyph IDs; returns a list of entries (with `NIL`
for glyphs that were blank or didn't fit).

### `atlas-add-text (atlas font text w h &key direction script language basic)`

Shapes `text` with `font`, deduplicates glyph IDs, and packs each unique
glyph at `w` x `h`. Returns a list of entries in first-seen order.

### `atlas-add-glyph-scaled (atlas font glyph-id pixels-per-em &optional (padding 2))`

Like `atlas-add-glyph`, but auto-sizes the bitmap from the glyph's own
outline bounds at `pixels-per-em` scale (e.g. `64`) instead of a fixed `w`/`h`
-- glyphs pack tightly instead of all sharing one cell size. `padding` here
is extra pixels around the glyph shape reserved for SDF range, not the
atlas's inter-glyph padding. Also records font-unit bounds on the entry (see
"Font-unit bounds" below).

### `atlas-add-chars (atlas font string pixels-per-em &key (padding 2) basic)`

Shapes `string`, deduplicates glyph IDs, and packs each via
`atlas-add-glyph-scaled`.

### `atlas-lookup (atlas glyph-id)`

Look up an already-packed glyph's `atlas-entry` by ID, or `NIL`.

## Reading back an entry

### `atlas-entry` accessors

- `atlas-entry-glyph-id` -- the glyph ID.
- `atlas-entry-region` -- an `atlas-region` (`atlas-region-x`/`-y`/`-width`/`-height`,
  pixel rectangle within the atlas bitmap).
- `atlas-entry-u0`/`-v0`/`-u1`/`-v1` -- normalized `[0,1]` UV rectangle
  corresponding to the region, for sampling the exported texture.
- `atlas-entry-fu-x0`/`-y0`/`-x1`/`-y1` -- **only meaningful for
  `atlas-add-glyph-scaled`/`atlas-add-chars` entries** -- the font-unit
  bounds of the rendered bitmap region, following the msdfgen convention
  (`shape_x = (pixel_x + 0.5 + tx) / scale`). Use these to map the packed
  bitmap back onto the glyph's outline coordinate space, e.g. for computing
  per-glyph quad geometry at render time.

## Exporting

### `atlas-to-png (atlas file)`

Writes the atlas's full bitmap to a PNG at `file` (grayscale for
`:sdf`/`:bitmap`/`:bitmap-fast` modes, RGB for `:msdf`).

## Packing algorithm and atlas capacity

Glyphs are packed with a **skyline** algorithm: the atlas tracks a
horizontal "skyline" profile of occupied height across its width, and each
new glyph is placed at the position that minimizes wasted vertical space
(best-fit along the skyline), then the skyline is updated to reflect the new
occupied region.

The atlas does **not** grow or evict when full: if no position fits a
glyph's padded dimensions, the `atlas-add-*` functions return `NIL` for that
glyph (silently -- no error, no bitmap corruption) rather than resizing or
overwriting an existing entry. Check for `NIL` in the returned entry list if
you need to know whether every glyph was actually packed; if so, create a
larger atlas (bigger `width`/`height`) and re-pack.

## Example

```lisp
(ql:quickload :harfarasta/fontstash)

(rich-text:with-font (font "/path/to/font.ttf")
  (let ((atlas (harfarasta/fontstash:make-font-atlas
                :width 256 :height 256 :mode :sdf :padding 2)))
    (let ((entries (harfarasta/fontstash:atlas-add-text
                    atlas font "ABCabc123" 32 32)))
      (format t "Packed ~D glyphs~%" (count-if #'identity entries))
      (dolist (e entries)
        (when e
          (let ((r (harfarasta/fontstash:atlas-entry-region e)))
            (format t "glyph ~D @ (~D,~D) ~Dx~D  UV (~,3F,~,3F)-(~,3F,~,3F)~%"
                    (harfarasta/fontstash:atlas-entry-glyph-id e)
                    (harfarasta/fontstash:atlas-region-x r)
                    (harfarasta/fontstash:atlas-region-y r)
                    (harfarasta/fontstash:atlas-region-width r)
                    (harfarasta/fontstash:atlas-region-height r)
                    (harfarasta/fontstash:atlas-entry-u0 e)
                    (harfarasta/fontstash:atlas-entry-v0 e)
                    (harfarasta/fontstash:atlas-entry-u1 e)
                    (harfarasta/fontstash:atlas-entry-v1 e)))))
      (harfarasta/fontstash:atlas-to-png atlas "atlas-sdf.png"))))
```
