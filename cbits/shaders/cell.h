/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
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
/* Allocated widths1/2 and clip offsets retain their original representation.
 * Script geometry is independent of paint and shaped atlas identity. */
#define HIDE_CELL_WIDTH_MASK 3u
#define HIDE_CELL_NATURAL_SHIFT 2u
#define HIDE_CELL_SCRIPT_SHIFT 4u
#define HIDE_CELL_SCRIPT_MASK 3u
/* Atlas rectangles retain full glyph geometry. Normal cells have no predecessor;
 * only zero-advance combining overlays use an append-only tail, at most 16 deep. */
struct HideGlyphCell {
    HIDE_UINT4 geometry; /* packed atlas x/y, width/height, allocated2/natural2/script2 bits, clip offset16, predecessor+1 */
    HIDE_UINT4 paint; /* foreground RGB, background RGB, background/bitmap flags + underline8/strike16, overlay depth */
};
#undef HIDE_UINT4
#endif
