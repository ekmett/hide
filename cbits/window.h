#ifndef THC_WINDOW_H
#define THC_WINDOW_H
#include <stdint.h>
void thc_post_command(int command);
int thc_open(const char *backend, int scale, int cols, int rows, int cell_height);
int thc_mode(int cell_height, int cols, int rows);
/* Grow/shrink tiles without changing the grid; zero restores the density-aware default. */
int thc_scale(int direction);
void thc_close(void);
const char *thc_error(void);
const char *thc_backend(void);
void thc_size(int *cols, int *rows);
int thc_begin(void);
void thc_glyph(int x, int y, int cells, int glyph_width, const uint16_t *rows, uint32_t fg, uint32_t bg);
void thc_cursor(int x, int y);
void thc_cursor_blink(int enabled);
int thc_present(void);
/* Six integers; kind 0 is a 100ms idle wake, kind 8 requests a redraw (also blink),
 * and kind 12 is hover at cell x,y.
 * Button-down kind 3 has click count in slot 3 and SDL button number in slot 5.
 * Hover (-1,-1) leaves the window. A zero return indicates an SDL error. */
int thc_wait(int32_t *event);
const char *thc_text(void);
const char *thc_clipboard(void);
void thc_set_clipboard(const char *text);
int thc_capture(const char *path);
#endif
