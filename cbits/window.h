#ifndef THC_WINDOW_H
#define THC_WINDOW_H
#include <stddef.h>
#include <stdint.h>
void thc_post_command(int command, int generation);
void thc_post_window(int ident, int generation);
void thc_raise(void);
#ifdef __APPLE__
void thc_dock_close(void);
void thc_dock_raise(void *native_window);
void thc_file_drag_arm(void *native_window, const char *path, double x, double y, double width, double height);
void thc_file_drag_close(void);
void thc_file_drag_ended(void);
#endif
/* Thread-safe wake after queuing an incoming frame; no repaint implied. */
void thc_wake(void);
int thc_open(const char *backend, double scale, int cols, int rows, int cell_height);
int thc_mode(int cell_height, int cols, int rows);
/* Grow/shrink tiles without changing the grid; zero restores the density-aware default. */
int thc_scale(int direction);
void thc_title(const char *text);
void thc_close(void);
/* Main-thread read-only sidebar snapshot; NULL/0 clears. Non-macOS is a no-op. */
int thc_accessibility(const char *json, size_t length);
const char *thc_error(void);
const char *thc_backend(void);
void thc_size(int *cols, int *rows);
int thc_begin(void);
/* SDL-thread retained straight-RGBA resources. One contiguous upload cursor;
 * complete scene commits retain only a normal-cell ownership stencil. */
int thc_canvas_reset(const char *epoch);
int thc_canvas_begin(const char *epoch, const char *id, int width, int height, size_t bytes);
/* Chunk returns 2 on completion, 1 while incomplete, 0 on error. */
int thc_canvas_chunk(const char *epoch, const char *id, size_t offset, const void *bytes, size_t length);
int thc_canvas_release(const char *epoch, const char *id);
/* Zero cells is the empty-scene shorthand and forbids any surface. */
int thc_canvas_scene(const char *epoch, int cols, int rows, const void *little_endian_mask, size_t cells);
int thc_canvas_surface(const char *id, int slot, int x, int y, int width, int height, double tx, double ty, double tw, double th);
void thc_canvas_clear(void);
int thc_canvas_commit(void);
void thc_canvas_stats(uint64_t *uploads, uint64_t *bytes, uint64_t *masks, uint64_t *retained, uint64_t *draws);
/* The next glyph retains its full origin/width; only these visible cells draw. */
void thc_clip(int visible_x, int clip_cells);
/* Traits: bold1, italic2, explicit-width4, underline8, strikethrough16.
 * Line decorations are cell paint and never alter glyph atlas identity.
 * Script0 is baseline; script1/2 is upper/lower half-size ink. Scripts allocate
 * one cell and retain natural_cells1/2 for normal-resolution atlas sampling. */
void thc_glyph(int x, int y, int cells, int glyph_width, const uint16_t *rows, uint32_t fg, uint32_t bg, uint32_t traits, int script, int natural_cells);
void thc_pixelate_unicode(int enabled);
int thc_unicode(int x, int y, int cells, const char *text, uint32_t fg, uint32_t bg, uint32_t traits, int script, int natural_cells);
void thc_cursor(int x, int y);
void thc_cursor_blink(int enabled);
void thc_crt_filter(int enabled);
int thc_present(void);
/* Six integers; kind 0 is a 100ms idle wake, kind 8 requests a redraw (also blink),
 * kind 12 is hover at cell x,y, and kind 13 carries held modifier bits in slot 1.
 * Wheel kind 9 carries signed detents in slot 3, including coalesced travel.
 * Button-down kind 3 has click count in slot 3 and SDL button number in slot 5.
 * Hover (-1,-1) leaves the window. A zero return indicates an SDL error. */
int thc_wait(int32_t *event);
uint64_t thc_event_age_ns(void);
void thc_atlas_stats(uint64_t *uploads, uint64_t *bytes, uint64_t *batches);
void thc_grid_stats(uint64_t *uploads, uint64_t *bytes);
const char *thc_text(void);
const char *thc_clipboard(void);
void thc_set_clipboard(const char *text);
/* Arm a copy gesture in a captured file row; native mouse input owns the drag. */
void thc_cancel_file_drag(void);
int thc_arm_file_drag(const char *path, int x, int y, int width, int height);
int thc_capture(const char *path);
int thc_system_dark(void);
#endif
