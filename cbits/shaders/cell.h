/* SPDX-License-Identifier: BSD-3-Clause */
#ifndef HIDE_GLYPH_CELL_H
#define HIDE_GLYPH_CELL_H
/* This project has a C host, so __STDC__ deliberately joins the C++ boundary. */
#if defined(__cplusplus) || defined(__STDC__)
#include <stdint.h>
struct HideUInt4 { uint32_t x,y,z,w; };
#define HIDE_UINT4 struct HideUInt4
#else
#define HIDE_UINT4 uint4
#endif
/* Atlas rectangles retain full glyph geometry. Normal cells have no predecessor;
 * only zero-advance combining overlays use an append-only tail, at most 16 deep. */
struct HideGlyphCell {
    HIDE_UINT4 geometry; /* packed atlas x/y, width/height, full cells/offset, predecessor+1 */
    HIDE_UINT4 paint; /* foreground RGB, background RGB, background/bitmap flags + underline8/strike16, overlay depth */
};
#undef HIDE_UINT4
#endif
