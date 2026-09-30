#ifndef THC_WINDOW_H
#define THC_WINDOW_H
#include <stdint.h>
void thc_post_command(int command);
int thc_open(const char *backend, int scale);
void thc_close(void);
const char *thc_error(void);
const char *thc_backend(void);
void thc_size(int *cols, int *rows);
int thc_begin(void);
void thc_glyph(int x, int y, int cells, int glyph_width, const uint16_t *rows, uint32_t fg, uint32_t bg);
void thc_cursor(int x, int y);
int thc_present(void);
int thc_wait(int32_t *event);
const char *thc_text(void);
const char *thc_clipboard(void);
void thc_set_clipboard(const char *text);
int thc_capture(const char *path);
#endif
