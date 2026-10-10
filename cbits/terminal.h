/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef THC_TERMINAL_H
#define THC_TERMINAL_H
#include <stddef.h>
#include <stdint.h>
#ifdef _WIN32
#include <wchar.h>
#endif

typedef struct thc_terminal thc_terminal;
/* ABI cell words: UTF-8 offset, byte length, RGB foreground, RGB background,
 * attributes (bold=1, italic=2, underline=4, strike=8, faint=16), width. */
thc_terminal *thc_terminal_new(int columns, int rows);
#ifdef _WIN32
int thc_terminal_spawn_windows(thc_terminal *, const wchar_t *executable, wchar_t *command,
                               const wchar_t *environment, const wchar_t *directory);
#else
int thc_terminal_spawn(thc_terminal *, const char *executable, char *const argv[],
                       char *const env[], const char *directory);
#endif
int thc_terminal_write(thc_terminal *, const uint8_t *, size_t);
enum thc_terminal_mouse_action {
    THC_TERMINAL_MOUSE_PRESS, THC_TERMINAL_MOUSE_RELEASE, THC_TERMINAL_MOUSE_MOTION
};
enum thc_terminal_mouse_button {
    THC_TERMINAL_MOUSE_NONE, THC_TERMINAL_MOUSE_LEFT, THC_TERMINAL_MOUSE_MIDDLE,
    THC_TERMINAL_MOUSE_RIGHT, THC_TERMINAL_MOUSE_WHEEL_UP, THC_TERMINAL_MOUSE_WHEEL_DOWN,
    THC_TERMINAL_MOUSE_WHEEL_LEFT, THC_TERMINAL_MOUSE_WHEEL_RIGHT
};
/* Zero-based cells; captured motion/release may be outside the current grid.
 * Modifiers: shift=1, control=2, alt=4. Wheels are press-only. */
int thc_terminal_mouse(thc_terminal *, int action, int button, int modifiers, int column, int row);
/* 0/1 for tracking disabled/enabled; -1 on native query failure. */
int thc_terminal_mouse_tracking(thc_terminal *);
int thc_terminal_resize(thc_terminal *, int columns, int rows);
int thc_terminal_appearance(thc_terminal *, int dark);
int thc_terminal_poll(thc_terminal *);
/* Parser-only entry point also supports deterministic native tests. */
void thc_terminal_feed(thc_terminal *, const uint8_t *, size_t);
const uint32_t *thc_terminal_cells(thc_terminal *);
const uint8_t *thc_terminal_text(thc_terminal *, size_t *length);
const uint8_t *thc_terminal_output(thc_terminal *, size_t *length);
int thc_terminal_pid(thc_terminal *);
/* columns, rows, cursor x/y (-1 hidden), exited, exit code */
void thc_terminal_info(thc_terminal *, int info[6]);
const char *thc_terminal_error(thc_terminal *);
void thc_terminal_kill(thc_terminal *);
void thc_terminal_free(thc_terminal *);
#endif
