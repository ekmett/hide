/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
/* Disposable SDL host for the actual Unix menu adapter. The AT-SPI client owns
 * each transition through this pipe; no editor actions or private data enter it. */
#include "accessibility-menu.h"
#include "window.h"
#include <SDL3/SDL.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
int main(void) {
    assert(thc_open("software",1,80,25,16));
    int window_count=0;
    SDL_Window **windows=SDL_GetWindows(&window_count);
    assert(window_count==1);
    SDL_Window *window=windows[0];SDL_free(windows);
    assert(window);
    assert(thc_begin() && thc_present());
    setvbuf(stdout,NULL,_IOLBF,0);
    puts("READY");
    char command[32];
    int window_closed=0;
    while (fgets(command,sizeof(command),stdin)) {
        SDL_PumpEvents();
        if (!strcmp(command,"QUIT\n")) break;
        if (!strcmp(command,"FREE\n")) { thc_close();window_closed=1;puts("FREED");continue; }
        thc_ax_menu_node nodes[]={
          {2,"menu:file",-1,THC_AX_MENU_ROOT,"File","",{8,16,200,100},1,0,-1},
          {3,"menu:file:open",0,THC_AX_MENU_ITEM,"Open fixture λ","Ctrl+O",{16,32,184,16},1,1,-1},
          {4,"menu:file:disabled",0,THC_AX_MENU_ITEM,"Disabled fixture","",{16,48,184,16},0,0,-1},
          {5,"menu:file:radio",0,THC_AX_MENU_RADIO,"Checked fixture","",{16,64,184,16},1,0,1},
          {6,"menu:file:separator",0,THC_AX_MENU_SEPARATOR,"","",{16,80,184,16},0,0,-1}};
        thc_ax_menu_snapshot snapshot={1,640,400,5,nodes};
        if (!strcmp(command,"CLOSE\n")) { snapshot.present=0;snapshot.count=0; }
        else if (!strcmp(command,"PRIVATE\n")) snapshot.count=0;
        else if (!strcmp(command,"REPLACE\n")) {
            nodes[0].token=12;nodes[0].id="menu:edit";nodes[0].name="Edit";
            nodes[1].token=13;nodes[1].id="menu:edit:copy";nodes[1].name="Replacement fixture";
            snapshot.count=2;
        } else if (!strcmp(command,"INVALID\n")) snapshot.width=0;
        else assert(!strcmp(command,"OPEN\n"));
        int published=thc_accessibility_menu_begin(snapshot.present,snapshot.width>0?80:0,25);
        if (published) for (size_t i=0;i<snapshot.count;i++) {
            const thc_ax_menu_node *node=&nodes[i];
            if (!thc_accessibility_menu_item(node->id,node->parent,node->role,node->name,node->shortcut,
                (int)node->bounds.x/8,(int)node->bounds.y/16,(int)node->bounds.width/8,(int)node->bounds.height/16,
                node->enabled,node->current,node->checked)) published=0;
        }
        int ended=thc_accessibility_menu_end();
        published=published && ended;
        assert(published==(strcmp(command,"INVALID\n")!=0));
        puts(published?"PUBLISHED":"REJECTED");
    }
    if (!window_closed) thc_close();
    return 0;
}
