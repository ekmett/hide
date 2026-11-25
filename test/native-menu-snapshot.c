/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#include "../cbits/accessibility-menu.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static thc_ax_menu_snapshot *adopted;
static int publishes;
int thc_ax_platform_menu_publish(const thc_ax_menu_snapshot *value) {
    ++publishes;
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
    assert(thc_ax_menu_begin(0,100,100) && thc_ax_menu_end());
    int absent_publishes=publishes;
    for (int i=0;i<20;++i) assert(thc_ax_menu_begin(0,100,100) && thc_ax_menu_end());
    assert(publishes==absent_publishes);
    assert(thc_ax_menu_begin(0,101,100) && thc_ax_menu_end() && publishes==absent_publishes+1);
    /* Admitted 256-node wire projection: 32644 scalars, below 262144 bytes. */
    char unicode[513],identity[80];
    for (size_t i=0;i<128;++i) memcpy(unicode+i*4,"\xf0\x9f\x99\x82",4);
    unicode[512]=0;
    assert(thc_ax_menu_begin(1,80,256));
    assert(thc_ax_menu_item("[\"menu\",\"file\"]",-1,THC_AX_MENU_ROOT,"Menu","",0,0,80,256,1,0,-1));
    for (int i=0;i<255;++i) {
        snprintf(identity,sizeof(identity),"[\"menu\",\"file\",\"builtin\",\"x\",\"%d\"]",i);
        assert(thc_ax_menu_item(identity,0,THC_AX_MENU_ITEM,unicode,"",1,1+i,60,1,1,i==0,-1));
    }
    assert(thc_ax_menu_end() && adopted->count==256);
    thc_ax_menu_clear();thc_ax_menu_snapshot_free(adopted);
    puts("Bounded native menu copies, live identity, retirement and invalid replacement checks passed");
}
