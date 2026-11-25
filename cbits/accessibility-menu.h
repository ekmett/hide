/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef HIDE_ACCESSIBILITY_MENU_H
#define HIDE_ACCESSIBILITY_MENU_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
#define THC_AX_MENU_MAX_NODES 256
#define THC_AX_MENU_MAX_TEXT_BYTES 262144
/* These are inert presentation roles, never action permissions. */
typedef enum {
    THC_AX_MENU_ROOT=0, THC_AX_MENU_ITEM=1,
    THC_AX_MENU_RADIO=2, THC_AX_MENU_SEPARATOR=3
} thc_ax_menu_role;
typedef struct { double x,y,width,height; } thc_ax_rect;
typedef struct {
    uint64_t token;                 /* Monotonic host identity, 1 is reserved. */
    const char *id;                 /* Opaque original projection identity. */
    int parent;                    /* -1 for menu root; otherwise node index. */
    thc_ax_menu_role role;
    const char *name;
    const char *shortcut;
    thc_ax_rect bounds;             /* Renderer-converted client pixels. */
    int enabled,current,checked;   /* checked: -1 absent, 0 false, 1 true. */
} thc_ax_menu_node;
typedef struct {
    int present;
    double width,height;            /* Client pixel extent. */
    size_t count;
    const thc_ax_menu_node *nodes;
} thc_ax_menu_snapshot;
/* Deep bounded copies; adapters retain a clone, never the caller's pointers. */
thc_ax_menu_snapshot *thc_ax_menu_snapshot_clone(const thc_ax_menu_snapshot *value);
void thc_ax_menu_snapshot_free(thc_ax_menu_snapshot *value);
/* SDL thread only. native_window is a borrowed SDL_Window*, never released. */
int thc_ax_platform_menu_open(void *native_window);
int thc_ax_platform_menu_publish(const thc_ax_menu_snapshot *value);
void thc_ax_platform_menu_geometry(void);
void thc_ax_platform_menu_focus(int focused);
void thc_ax_platform_menu_close(void);
/* Internal SDL-thread builder; public cell-coordinate entrypoints are window.h. */
int thc_ax_menu_begin(int present,double width,double height);
int thc_ax_menu_item(const char *id,int parent,int role,const char *name,const char *shortcut,
                    double x,double y,double width,double height,int enabled,int current,int checked);
int thc_ax_menu_end(void);
void thc_ax_menu_clear(void);
/* Platform callbacks read only copied immutable state; no SDL/Haskell calls. */
#ifdef __cplusplus
}
#endif
#endif
