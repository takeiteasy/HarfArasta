# Shaping & Font Management

The core `harfarasta` package (nickname `rich-text`) loads fonts, shapes text
via HarfBuzz, and exposes the shaped-glyph stream that every renderer
(SDF/MSDF, bitmap, mesh) and `harfarasta/export`/`harfarasta/mesh` consume.
This document covers font loading/lifecycle, shaping, word wrap, font
inspection, and fallback fonts. Rendering itself is documented separately
(see [`docs/rendering.md`](rendering.md), [`docs/triangulation.md`](triangulation.md),
[`docs/mesh.md`](mesh.md), [`docs/export.md`](export.md)).

## Font loading & lifecycle

Fonts are HarfBuzz `hb_font_t` pointers. There are two ways to manage their
lifetime: the scoped `with-font` macro (recommended for most use), or manual
`create-font`/`destroy-font` when a font must outlive a single dynamic scope
(e.g. held across multiple render calls, or used as a fallback font).

### `with-font (font-var first-arg &rest args) &body body`

Loads a font, binds it to `font-var`, and destroys it (blob/face/font) when
`body` exits. Four calling conventions:

```lisp
(with-font (f "/path/to/font.ttf") ...)              ; path mode
(with-font (f "/path/to/font.ttf" :index 1) ...)     ; path mode, face index
(with-font (f :family "Helvetica") ...)               ; discovery mode
(with-font (f :family "Roboto" :weight :bold) ...)    ; discovery mode
(with-font (f :bytes my-byte-vector) ...)             ; byte vector mode
(with-font (f :bytes my-byte-vector :index 1) ...)    ; byte vector mode
```

Discovery mode forwards its keywords to `find-font-path`. All modes
transparently decode WOFF1/WOFF2 if the source data is in that format (see
"WOFF1/WOFF2" below) and set the font's HarfBuzz scale to its units-per-em.

### `find-font-path (&key family weight slant spacing stretch)`

Finds a system font file matching the given criteria via
[`font-discovery`](https://shinmera.com/project/font-discovery) and returns
its pathname. Signals an error if nothing matches.

### `create-font (path &key (index 0))` / `create-font-from-bytes (bytes &key (index 0))`

Persistent (non-scoped) font loading, for fonts you manage yourself. Returns
`(values font face blob upem)` -- all three pointers must be passed to
`destroy-font` when done. Use these instead of `with-font` when a font needs
to live across multiple calls or be reused as a fallback font (`with-font`'s
`unwind-protect` would tear it down too early).

### `destroy-font (font face blob)`

Frees resources created by `create-font`/`create-font-from-bytes`.

### `identify-font-format (bytes)`

Detects font format from a byte vector's magic number. Returns `:ttf`,
`:otf`, `:woff1`, `:woff2`, or `NIL` if unrecognized.

## Shaping

### `shape-text (font text &key direction script language alignment line-height max-width (wrap :word) (fallback-fonts *fallback-fonts*) basic)`

Shapes `text` with `font` via HarfBuzz. Returns a list of `shaped-glyph`
structs.

| Key | Default | Description |
|-----|---------|-------------|
| `direction` | `NIL` (auto) | `:ltr`, `:rtl`, `:ttb`, `:btt` |
| `script` | `NIL` (auto) | 4-char OpenType script tag, e.g. `"Latn"` |
| `language` | `NIL` (auto) | BCP-47 string, e.g. `"en"` |
| `alignment` | `:left` | `:left`, `:center`, `:right` (multi-line layout) |
| `line-height` | font upem | Y distance between lines, in font units |
| `max-width` | `NIL` | triggers word wrapping (font units); see below |
| `wrap` | `:word` | `:word` or `:glyph` |
| `fallback-fonts` | `*fallback-fonts*` | list of `hb_font_t` tried for missing glyphs |
| `basic` | `NIL` | skip HarfBuzz shaping; see below |

Unset `direction`/`script`/`language` are guessed via
`hb_buffer_guess_segment_properties`. Multi-line text (containing
`#\Newline`) is shaped line-by-line and returned as a single flat list
containing synthetic "skip" glyphs that encode cursor jumps between lines --
consumers walk this list with the internal `%map-shaped-glyphs` helper (all
the `text-to-*` renderers already do this for you).

`basic`, when true, skips HarfBuzz's shaping engine entirely and maps each
character directly to its nominal glyph ID via `hb_font_get_nominal_glyph` +
default `hb_font_get_glyph_h_advance`. This is faster but gives up
ligatures, kerning, and BiDi reordering -- use it only when you know the text
doesn't need complex shaping.

### `shape-text-lines (font text &key ...)`

Same keys as `shape-text` (minus `basic`'s interaction with multi-line
internals). Returns a list of plists `(:y y-offset :x x-offset :glyphs
shaped-glyphs)`, one per line, already alignment-adjusted and stacked by
`line-height`. Useful when you want per-line access instead of the flat
skip-glyph-encoded list `shape-text` returns.

### `string-bounds (font text &key ... depth)`

Computes the ink bounding box of `text` without rendering it, in font units,
OBJ/3D convention (X rightward, Y upward, baseline at Y=0). Without `depth`:
returns `(values width height)`. With `depth`: returns `(values min-x min-y
min-z max-x max-y max-z)`. Returns zeros if `text` has no visible glyphs.

### The `shaped-glyph` struct

```lisp
(defstruct shaped-glyph
  glyph-id      ; fixnum
  cluster       ; fixnum, byte offset into the source text
  x-advance     ; fixnum, font units
  y-advance     ; fixnum, font units
  x-offset      ; fixnum, font units
  y-offset      ; fixnum, font units
  font          ; NIL = use the primary font passed to shape-text, else a fallback hb_font_t
  skip)         ; T = synthetic layout glyph (advance cursor, don't render)
```

Accessors: `shaped-glyph-glyph-id`, `shaped-glyph-cluster`,
`shaped-glyph-x-advance`, `shaped-glyph-y-advance`, `shaped-glyph-x-offset`,
`shaped-glyph-y-offset`, `shaped-glyph-font`, `shaped-glyph-skip`.

## Word wrap

`max-width` (font units) triggers automatic line breaking, independent of
any hard `#\Newline` already in the text (each hard-broken paragraph is
wrapped independently, so existing newlines are preserved). `wrap` selects
the algorithm:

- `:word` (default) -- greedy word-boundary wrapping; breaks only at spaces.
- `:glyph` -- breaks at any glyph boundary, allowing mid-word breaks. Useful
  for narrow columns or CJK text without spaces.

Word wrap requires shaping each candidate line to measure its width, so it
costs more than unwrapped shaping -- expect roughly one extra shape per line
break candidate.

## Font inspection

- `font-has-char-p (font char-or-codepoint)` -- `T` if `font` has a glyph
  for the given character or integer codepoint (checked via
  `hb_font_get_nominal_glyph`).
- `font-missing-chars (font string)` -- list of characters in `string` that
  `font` has no glyph for.
- `font-monospace-p (font)` -- `T` if the font's OpenType `post` table
  `isFixedPitch` field is set.

## Fallback fonts

- `*fallback-fonts*` -- dynamic variable, list of `hb_font_t` pointers tried
  in cluster order when a shaped glyph comes back with `glyph-id` 0
  (missing). `NIL` by default. You manage the lifetime of these fonts
  yourself (typically via `create-font`/`destroy-font`, since they usually
  need to outlive any single `with-font` scope on the primary font).
- `with-fallback-fonts (fonts) &body body` -- binds `*fallback-fonts*` for
  `body`'s dynamic extent.
- Every shaping entry point also accepts `:fallback-fonts` directly, which
  overrides `*fallback-fonts*` for that call.

Internally, missing glyphs are resolved by mapping the glyph's cluster back
to its source codepoint and trying `hb_font_get_nominal_glyph` against each
fallback font in order; the first hit replaces the glyph (with that font's
advance) and its `shaped-glyph-font` slot records which fallback font to
draw it with. Unresolved glyphs are left with `glyph-id` 0 (typically the
".notdef" box, or nothing, depending on the primary font).

## WOFF1/WOFF2

WOFF1 and WOFF2 are decoded transparently -- there's no separate API to
call. Any function that loads a font from a path or byte vector
(`with-font`, `create-font`, `create-font-from-bytes`) detects the format
from its magic bytes and decodes it to raw TTF/OTF before handing it to
HarfBuzz:

- WOFF1 decoding is pure Lisp (zlib inflate).
- WOFF2 decoding goes through the `harfarasta/woff2` CFFI bindings to
  libwoff2shim (built alongside the HarfBuzz shared library).

You can also call `identify-font-format` directly if you need to know a
font's format ahead of loading it.

## Example

```lisp
(rich-text:with-font (font "/path/to/font.ttf")
  (format t "monospace: ~A~%" (rich-text:font-monospace-p font))
  (format t "missing chars: ~A~%" (rich-text:font-missing-chars font "Hello 日本語"))

  (let ((glyphs (rich-text:shape-text font "Hello, World!"
                                       :max-width 200 :wrap :word)))
    (dolist (g glyphs)
      (unless (rich-text:shaped-glyph-skip g)
        (format t "glyph=~D advance=~D~%"
                (rich-text:shaped-glyph-glyph-id g)
                (rich-text:shaped-glyph-x-advance g)))))

  (multiple-value-bind (w h) (rich-text:string-bounds font "Hello, World!")
    (format t "ink bounds: ~Dx~D~%" w h)))
```
