/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef THC_UNICODE_H
#define THC_UNICODE_H
#include <stdint.h>
/* Pure stateful boundary step: low bit is break, remaining bits retain the 32-bit utf8proc state. */
uint64_t thc_grapheme_step(int previous, int current, int state);
/* Rasterize a whole cluster to premultiplied ARGB, with system font fallback. */
int thc_unicode_bitmap(const char *utf8, int width, int height, uint32_t fg, uint32_t traits, uint32_t *pixels);
/* Four-times oversampling followed by area filtering and error diffusion. */
int thc_unicode_pixelated(const char *utf8, int width, int height, uint32_t fg, uint32_t traits, uint32_t *pixels);
int thc_unicode_downsample(const uint32_t *source, int width, int height, uint32_t *pixels);
#endif
