#define SDL_MAIN_HANDLED
#include <windows.h>
#include <SDL3/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "accessibility-menu.h"
static Uint32 commands;
static DWORD WINAPI input(void *unused) {
    (void)unused;char line[80];
    while (fgets(line,sizeof(line),stdin)) {
        SDL_Event event={0};event.type=commands;
        event.user.code=!strncmp(line,"rename",6)?1:!strncmp(line,"clear",5)?2:!strncmp(line,"close",5)?3:!strncmp(line,"reopen",6)?4:!strncmp(line,"invalid",7)?5:9;
        SDL_PushEvent(&event);if(event.user.code==9) return 0;
    }
    SDL_Event event={0};event.type=commands;event.user.code=9;SDL_PushEvent(&event);return 0;
}
static int publish(const char *label) {
    if (!thc_ax_menu_begin(1,320,240)) return 0;
    if (!thc_ax_menu_item("menu",-1,THC_AX_MENU_ROOT,"Owned menu","",0,0,200,100,1,0,-1)) return 0;
    if (!thc_ax_menu_item("item",0,THC_AX_MENU_ITEM,label,"Ctrl+O",10,20,170,20,0,1,-1)) return 0;
    if (!thc_ax_menu_item("radio",0,THC_AX_MENU_RADIO,"Current","",10,40,170,20,1,0,1)) return 0;
    return thc_ax_menu_end();
}
int main(void) {
    SetErrorMode(SEM_FAILCRITICALERRORS|SEM_NOGPFAULTERRORBOX);setvbuf(stdout,NULL,_IONBF,0);
    if(!SDL_Init(SDL_INIT_VIDEO)) {fprintf(stderr,"SDL init: %s\n",SDL_GetError());return 10;}
    SDL_Window *window=SDL_CreateWindow("Opaque owned SDL host",320,240,SDL_WINDOW_HIDDEN);
    if(!window) {fprintf(stderr,"SDL window: %s\n",SDL_GetError());return 11;}
    if(!thc_ax_platform_menu_open(window) || !publish("First")) return 12;
    HWND hwnd=SDL_GetPointerProperty(SDL_GetWindowProperties(window),SDL_PROP_WINDOW_WIN32_HWND_POINTER,NULL);
    printf("READY %llu %lu %lu\n",(unsigned long long)(uintptr_t)hwnd,GetCurrentProcessId(),GetCurrentThreadId());
    commands=SDL_RegisterEvents(1);HANDLE reader=CreateThread(NULL,0,input,NULL,0,NULL);
    int code=0;SDL_Event event;
    while(SDL_WaitEvent(&event)) {
        if(event.type!=commands) continue;
        code=event.user.code;
        if(code==9) break;
        int result=1;
        if(code==1) result=publish("Renamed");
        if(code==2) thc_ax_menu_clear();
        if(code==3) {thc_ax_menu_clear();thc_ax_platform_menu_close();}
        if(code==4) result=thc_ax_platform_menu_open(window) && publish("New attachment");
        if(code==5) {thc_ax_menu_snapshot bad={1,0,240,0,NULL};result=!thc_ax_platform_menu_publish(&bad);}
        printf("ACK %d %d\n",code,result);
    }
    thc_ax_menu_clear();thc_ax_platform_menu_close();SDL_DestroyWindow(window);SDL_Quit();
    WaitForSingleObject(reader,5000);CloseHandle(reader);return code==9?0:13;
}
