/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#if defined(_WIN32) && defined(WITH_ACCESSKIT)
#include <windows.h>
#include <SDL3/SDL.h>
#include "accessibility-menu.h"
#include "accessibility-accesskit.h"

/* Slots/mutex outlive all COM references. Userdata is an attachment receipt,
 * never an SDL window, snapshot or reclaimable host object. */
static SRWLOCK menu_lock=SRWLOCK_INIT;
static thc_ax_menu_snapshot *menu_snapshot;
static uintptr_t attachment,next_attachment=1;
static accesskit_windows_subclassing_adapter *menu_adapter;
static accesskit_tree_update *menu_tree(void *userdata) {
    uintptr_t receipt=(uintptr_t)userdata;
    AcquireSRWLockShared(&menu_lock);
    const thc_ax_menu_snapshot *snapshot=receipt && receipt==attachment?menu_snapshot:NULL;
    accesskit_tree_update *tree=thc_ax_menu_accesskit_tree(snapshot);
    ReleaseSRWLockShared(&menu_lock);
    return tree;
}
static void no_action(accesskit_action_request *request,void *userdata) {
    (void)userdata;accesskit_action_request_free(request);
}
static void raise_update(accesskit_windows_subclassing_adapter *adapter,uintptr_t receipt) {
    accesskit_windows_queued_events *events=accesskit_windows_subclassing_adapter_update_if_active(adapter,menu_tree,(void *)receipt);
    if (events) accesskit_windows_queued_events_raise(events);
}
int thc_ax_platform_menu_open(void *native_window) {
    if (menu_adapter) thc_ax_platform_menu_close();
    SDL_Window *window=native_window;
    HWND hwnd=window?SDL_GetPointerProperty(SDL_GetWindowProperties(window),SDL_PROP_WINDOW_WIN32_HWND_POINTER,NULL):NULL;
    if (!hwnd || IsWindowVisible(hwnd) || GetFocus()==hwnd || next_attachment==UINTPTR_MAX) return 0;
    uintptr_t receipt=next_attachment++;
    AcquireSRWLockExclusive(&menu_lock);
    thc_ax_menu_snapshot *old=menu_snapshot;menu_snapshot=NULL;attachment=receipt;
    ReleaseSRWLockExclusive(&menu_lock);thc_ax_menu_snapshot_free(old);
    menu_adapter=accesskit_windows_subclassing_adapter_new(hwnd,menu_tree,(void *)receipt,no_action,NULL);
    if (!menu_adapter) { AcquireSRWLockExclusive(&menu_lock);attachment=0;ReleaseSRWLockExclusive(&menu_lock);return 0; }
    return 1;
}
int thc_ax_platform_menu_publish(const thc_ax_menu_snapshot *value) {
    thc_ax_menu_snapshot *copy=thc_ax_menu_snapshot_clone(value);
    if (!menu_adapter) { thc_ax_menu_snapshot_free(copy);return 0; }
    AcquireSRWLockExclusive(&menu_lock);
    thc_ax_menu_snapshot *old=menu_snapshot;menu_snapshot=copy;
    uintptr_t receipt=attachment;
    ReleaseSRWLockExclusive(&menu_lock);
    thc_ax_menu_snapshot_free(old);
    if (menu_adapter) raise_update(menu_adapter,receipt);
    return copy!=NULL;
}
void thc_ax_platform_menu_geometry(void) {
    /* The subclassing adapter resolves HWND/client screen geometry itself. */
}
void thc_ax_platform_menu_focus(int focused) {
    /* WM_SETFOCUS/WM_KILLFOCUS are owned by the subclassing adapter. */
    (void)focused;
}
void thc_ax_platform_menu_close(void) {
    accesskit_windows_subclassing_adapter *old_adapter=menu_adapter;menu_adapter=NULL;
    AcquireSRWLockExclusive(&menu_lock);
    uintptr_t receipt=attachment;attachment=0;
    thc_ax_menu_snapshot *old=menu_snapshot;menu_snapshot=NULL;
    ReleaseSRWLockExclusive(&menu_lock);
    thc_ax_menu_snapshot_free(old);
    if (old_adapter) {
        /* Withdraw children before freeing. Retained providers and delayed
         * callbacks can only obtain an empty tree for this retired receipt. */
        raise_update(old_adapter,receipt);
        accesskit_windows_subclassing_adapter_free(old_adapter);
    }
}
#endif
