#ifndef THC_UNICODE_H
#define THC_UNICODE_H
#include <stdint.h>
/* Character offsets of extended grapheme boundaries; room for characters+1. */
int thc_graphemes(const char *utf8, int bytes, int *boundaries);
/* Rasterize a whole cluster to premultiplied ARGB, with system font fallback. */
int thc_unicode_bitmap(const char *utf8, int width, int height, uint32_t fg, uint32_t *pixels);
#endif
