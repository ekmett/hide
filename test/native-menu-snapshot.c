/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#include "../cbits/accessibility-menu.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static thc_ax_menu_snapshot *adopted;
int thc_ax_platform_menu_publish(const thc_ax_menu_snapshot *value) {
    thc_ax_menu_snapshot *copy=thc_ax_menu_snapshot_clone(value);
    if (!copy) return 0;
    thc_ax_menu_snapshot_free(adopted);adopted=copy;return 1;
}
static void root(void) { assert(thc_ax_menu_item("root",-1,THC_AX_MENU_ROOT,"Menu","",0,0,90,90,1,0,-1)); }
static void item(const char *name) { assert(thc_ax_menu_item("item",0,THC_AX_MENU_ITEM,name,"Ctrl+O",1,1,20,1,1,1,-1)); }
int main(void) {
    char text[]="First";
    assert(thc_ax_menu_begin(1,100,100));root();item(text);text[0]='X';assert(thc_ax_menu_end());
    assert(adopted && adopted->count==2 && !strcmp(adopted->nodes[1].name,"First"));
    uint64_t token=adopted->nodes[1].token;
    assert(thc_ax_menu_begin(1,100,100));root();item("Second");assert(thc_ax_menu_end());
    assert(adopted->nodes[1].token==token && !strcmp(adopted->nodes[1].name,"Second"));
    thc_ax_menu_clear();assert(!adopted->present && !adopted->count);
    assert(thc_ax_menu_begin(1,100,100));root();item("Third");assert(thc_ax_menu_end());
    assert(adopted->nodes[1].token!=token);
    assert(thc_ax_menu_begin(1,100,100));root();
    assert(thc_ax_menu_item("bad",0,THC_AX_MENU_ITEM,"Hidden","",95,1,3,1,1,0,-1));
    assert(!thc_ax_menu_end() && !adopted->present && !adopted->count);
    assert(thc_ax_menu_begin(1,100,100));root();
    assert(thc_ax_menu_item("root",0,THC_AX_MENU_ITEM,"Duplicate","",1,1,3,1,1,0,-1));
    assert(!thc_ax_menu_end() && !adopted->present && !adopted->count);
    assert(!thc_ax_menu_begin(1,0,100) && !adopted->count);
    thc_ax_menu_clear();thc_ax_menu_snapshot_free(adopted);
    puts("Bounded native menu copies, live identity, retirement and invalid replacement checks passed");
}
