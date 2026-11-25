/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
/* Read-only painted menus. AccessKit callbacks run on its bus thread and read
 * only owned metadata; SDL and the desktop remain on their owning thread. */
#include "accessibility-menu.h"
#include "accessibility-accesskit.h"
#include <SDL3/SDL.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    accesskit_unix_adapter *adapter;
    SDL_Window *window;             /* borrowed, SDL thread only */
    thc_ax_menu_snapshot *snapshot;
    uintptr_t cookie;
    int focused,bounds_valid;
    double bounds_key[8];
} MenuAdapter;
static MenuAdapter *current;
static pthread_mutex_t menu_lock=PTHREAD_MUTEX_INITIALIZER;
static uintptr_t next_cookie;
/* Upstream adapter_free enqueues removal rather than joining its bus thread.
 * Callback userdata is therefore a scalar incarnation, never an owner pointer. */

static accesskit_tree_update *build_tree(void *userdata) {
    uintptr_t cookie=(uintptr_t)userdata;
    pthread_mutex_lock(&menu_lock);
    MenuAdapter *owner=current;
    accesskit_tree_update *update=thc_ax_menu_accesskit_tree(owner && owner->cookie==cookie?owner->snapshot:NULL);
    pthread_mutex_unlock(&menu_lock);
    return update;
}
static void refuse_action(accesskit_action_request *request,void *userdata) {
    (void)userdata;
    /* No action is advertised, and unsolicited requests never enter SDL. */
    accesskit_action_request_free(request);
}
static void deactivated(void *userdata) { (void)userdata; }

void thc_ax_platform_menu_focus(int focused) {
    if (current && current->focused!=(focused!=0)) {
        current->focused=focused!=0;
        accesskit_unix_adapter_update_window_focus_state(current->adapter,current->focused);
    }
}
void thc_ax_platform_menu_geometry(void) {
    MenuAdapter *owner=current;
    if (!owner) return;
    /* Wayland does not expose global window positions. Do not fabricate them. */
    if (strcmp(SDL_GetCurrentVideoDriver(),"x11")) return;
    int x=0,y=0,w=0,h=0,top=0,left=0,bottom=0,right=0,pw=0,ph=0;
    if (!SDL_GetWindowPosition(owner->window,&x,&y) || !SDL_GetWindowSize(owner->window,&w,&h) ||
        !SDL_GetWindowSizeInPixels(owner->window,&pw,&ph) || w<=0 || h<=0) return;
    (void)SDL_GetWindowBordersSize(owner->window,&top,&left,&bottom,&right);
    double sx=(double)pw/w,sy=(double)ph/h;
    accesskit_rect outer={(x-left)*sx,(y-top)*sy,(x+w+right)*sx,(y+h+bottom)*sy};
    accesskit_rect inner={x*sx,y*sy,(x+w)*sx,(y+h)*sy};
    double key[]={outer.x0,outer.y0,outer.x1,outer.y1,inner.x0,inner.y0,inner.x1,inner.y1};
    int same=owner->bounds_valid;
    for (size_t i=0;i<8;i++) if (owner->bounds_key[i]!=key[i]) same=0;
    if (!same) {
        memcpy(owner->bounds_key,key,sizeof(key));owner->bounds_valid=1;
        accesskit_unix_adapter_set_root_window_bounds(owner->adapter,outer,inner);
    }
}
void thc_ax_platform_menu_close(void) {
    pthread_mutex_lock(&menu_lock);
    MenuAdapter *owner=current;
    current=NULL; /* Retire the callback incarnation before any cleanup. */
    pthread_mutex_unlock(&menu_lock);
    if (!owner) return;
    accesskit_unix_adapter_update_if_active(owner->adapter,build_tree,(void *)owner->cookie);
    accesskit_unix_adapter_free(owner->adapter);
    thc_ax_menu_snapshot_free(owner->snapshot);
    free(owner);
}
int thc_ax_platform_menu_open(void *native_window) {
    if (!native_window) { thc_ax_platform_menu_close();return 0; }
    if (current && current->window==native_window) return 1;
    thc_ax_platform_menu_close();
    MenuAdapter *owner=calloc(1,sizeof(*owner));
    if (!owner) return 0;
    if (next_cookie==UINTPTR_MAX) { free(owner);return 0; }
    owner->window=native_window;
    owner->cookie=++next_cookie;
    pthread_mutex_lock(&menu_lock);
    current=owner;
    pthread_mutex_unlock(&menu_lock);
    owner->adapter=accesskit_unix_adapter_new(build_tree,(void *)owner->cookie,refuse_action,NULL,deactivated,NULL);
    if (!owner->adapter) {
        pthread_mutex_lock(&menu_lock);current=NULL;pthread_mutex_unlock(&menu_lock);
        free(owner);return 0;
    }
    thc_ax_platform_menu_geometry();
    thc_ax_platform_menu_focus((SDL_GetWindowFlags(owner->window)&SDL_WINDOW_INPUT_FOCUS)!=0);
    return 1;
}
int thc_ax_platform_menu_publish(const thc_ax_menu_snapshot *value) {
    MenuAdapter *owner=current;
    if (!owner) return 0;
    thc_ax_menu_snapshot *next=thc_ax_menu_snapshot_clone(value);
    /* Invalid metadata also withdraws the previously published menu. */
    pthread_mutex_lock(&menu_lock);
    thc_ax_menu_snapshot *old=owner->snapshot;
    owner->snapshot=next;
    pthread_mutex_unlock(&menu_lock);
    thc_ax_menu_snapshot_free(old);
    accesskit_unix_adapter_update_if_active(owner->adapter,build_tree,(void *)owner->cookie);
    return next!=NULL;
}
