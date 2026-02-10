# cl-rich-text Roadmap

## Phase 1: HarfBuzz CFFI Bindings [DONE]
Bind ~44 core HarfBuzz functions via CFFI:
- Blob: `hb_blob_create`, `hb_blob_destroy`, etc.
- Face: `hb_face_create`, `hb_face_destroy`, `hb_face_get_glyph_count`, etc.
- Font: `hb_font_create`, `hb_font_destroy`, `hb_font_set_scale`, etc.
- Buffer: `hb_buffer_create`, `hb_buffer_destroy`, `hb_buffer_add_utf8`, `hb_buffer_set_direction`, `hb_buffer_set_script`, `hb_buffer_set_language`, `hb_buffer_get_glyph_infos`, `hb_buffer_get_glyph_positions`, etc.
- Shape: `hb_shape`
- Draw: `hb_font_draw_glyph`, `hb_draw_funcs_create`, `hb_draw_funcs_set_*_func`

## Phase 2: Glyph Outline Extraction [DONE]
Wire HarfBuzz draw callbacks to `trivial-sdf:path-builder` to produce `shape` objects from glyph outlines.

## Phase 3: Single Glyph Rendering
- Bitmap rendering (regular, SDF, MSDF) via `trivial-signed-distance-fields`
- Vertex mesh generation (CDT) via `trivial-delaunay-triangulation`

## Phase 4: String Shaping + Rendering
Use `hb_shape` to get positioned glyphs, then render each glyph with appropriate offsets.

## Phase 5: Glyph Cache
Cache rendered glyphs keyed by `(glyph-id, font-size, render-mode)`.

## Phase 6: Polish
Documentation, optimization, error handling.
