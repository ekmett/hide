#ifndef THC_UNICODE_H
#define THC_UNICODE_H
#include <stdint.h>
/* UTF8 byte offsets of extended grapheme boundaries; room for characters+1. */
int thc_graphemes(const char *utf8, int bytes, int *boundaries);
/* Rasterize a whole cluster to premultiplied ARGB, with system font fallback. */
int thc_unicode_bitmap(const char *utf8, int width, int height, uint32_t fg, uint32_t traits, uint32_t *pixels);
/* Four-times oversampling followed by area filtering and error diffusion. */
int thc_unicode_pixelated(const char *utf8, int width, int height, uint32_t fg, uint32_t traits, uint32_t *pixels);
int thc_unicode_downsample(const uint32_t *source, int width, int height, uint32_t *pixels);
#endif
