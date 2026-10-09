/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef HIDE_ACCESSIBILITY_H
#define HIDE_ACCESSIBILITY_H
#include <stddef.h>
#ifdef __APPLE__
/* Main-thread read-only Cocoa snapshots; native_window is borrowed from SDL.
 * A present dialog suppresses sidebar/images, including privacy-hidden dialogs. */
int thc_accessibility_update(void *native_window, const char *json, size_t length);
void thc_accessibility_close(void);
void thc_accessibility_geometry_changed(void);
/* Exact renderer-owned cell edges converted to top-left content points. */
int thc_accessibility_cell_rect(int x, int y, int width, int height, double rectangle[4]);
#endif
#endif
