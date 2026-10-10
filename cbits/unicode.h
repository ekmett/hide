/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef THC_UNICODE_H
#define THC_UNICODE_H
#include <stdint.h>
/* Pure stateful boundary step: low bit is break, remaining bits retain the 32-bit utf8proc state. */
uint64_t thc_grapheme_step(int previous, int current, int state);
/* Window-only rasterization, supplied by unicode-window.c. Rasterize a whole
 * cluster to premultiplied ARGB with system font fallback. Input text and the
 * caller-owned width*height output pixels are borrowed until return. */
int thc_unicode_bitmap(const char *utf8, int width, int height, uint32_t fg, uint32_t traits, uint32_t *pixels);
/* Four-times oversampling followed by area filtering and error diffusion.
 * Borrowed buffers and success result match thc_unicode_bitmap. */
int thc_unicode_pixelated(const char *utf8, int width, int height, uint32_t fg, uint32_t traits, uint32_t *pixels);
/* Read borrowed (width*4)*(height*4) premultiplied ARGB pixels and write
 * width*height caller-owned pixels. Returns nonzero on success. */
int thc_unicode_downsample(const uint32_t *source, int width, int height, uint32_t *pixels);
#endif
