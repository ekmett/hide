/* SPDX-License-Identifier: BSD-3-Clause */
/* Hidden renderer execution, cell/atlas counters and full-origin clipping. */
#include "../cbits/window.h"
#include <SDL3/SDL.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#ifdef __APPLE__
void thc_dock_close(void) {}
void thc_dock_raise(void *window) { (void)window; }
#endif

static void scene(void) {
    uint16_t glyph[16];
    for (int y=0;y<16;++y) glyph[y]=0x9000;
    assert(thc_begin());
    for (int y=0;y<25;++y) for (int x=0;x<80;++x)
        thc_glyph(x,y,1,8,glyph,0xffffff,0x0000aa,0);
    assert(thc_unicode(3,2,2,"👩🏽‍💻",0xffffff,0x0000aa,0));
    assert(thc_unicode(6,2,1,"é",0xffff55,0x0000aa,3));
    thc_glyph(10,2,2,8,glyph,0xffff55,0x0000aa,4);
}
static void decoration_scene(uint32_t lines) {
    uint16_t blank[16]={0};
    assert(thc_begin());
    for (int y=0;y<25;++y) for (int x=0;x<80;++x)
        thc_glyph(x,y,1,8,blank,0xffffff,0x0000aa,0);
    thc_glyph(0,0,1,8,blank,0xffffff,0x0000aa,lines&8);
    thc_glyph(2,0,1,8,blank,0xffffff,0x0000aa,lines&16);
    thc_glyph(4,0,1,8,blank,0xffffff,0x0000aa,lines);
    thc_glyph(8,0,2,8,blank,0xffffff,0x0000aa,4|lines);
    thc_clip(12,1); thc_glyph(11,0,2,8,blank,0xffffff,0x0000aa,4|lines);
    thc_clip(14,1); thc_glyph(14,0,2,8,blank,0xffffff,0x0000aa,4|lines);
    assert(thc_unicode(17,0,2," ",0xffffff,0x0000aa,7|lines));
}
static void pixel_is(SDL_Surface *image,int x,int y,Uint8 r,Uint8 g,Uint8 b) {
    Uint8 actual_r,actual_g,actual_b,a;
    assert(SDL_ReadSurfacePixel(image,x,y,&actual_r,&actual_g,&actual_b,&a));
    if (actual_r!=r || actual_g!=g || actual_b!=b) fprintf(stderr,"pixel (%d,%d) in %dx%d: expected %u,%u,%u; actual %u,%u,%u\n",x,y,image->w,image->h,r,g,b,actual_r,actual_g,actual_b);
    assert(actual_r==r && actual_g==g && actual_b==b);
}
int main(int argc,char **argv) {
    const char *backend=argc>1?argv[1]:"software";
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE_EXIT","1",true);
    if (!strcmp(backend,"software")) SDL_SetHint(SDL_HINT_VIDEO_DRIVER,"dummy");
    if (!thc_open(backend,2,80,25,16)) { fprintf(stderr,"open: %s\n",thc_error()); return 1; }
    /* A hidden Cocoa window can inherit SDL's initial mouse focus at (0,0).
     * This scene measures glyph paint, so explicitly place the pointer outside. */
    SDL_FlushEvents(SDL_EVENT_FIRST,SDL_EVENT_LAST);
    SDL_Event leave; SDL_zero(leave); leave.type=SDL_EVENT_WINDOW_MOUSE_LEAVE; assert(SDL_PushEvent(&leave));
    int32_t event[6]; assert(thc_wait(event) && event[0]==12);
#ifndef HIDE_BASELINE
    SDL_Event expose; SDL_zero(expose); expose.type=SDL_EVENT_WINDOW_EXPOSED;
    expose.common.timestamp=SDL_GetTicksNS()-1000; assert(SDL_PushEvent(&expose));
    assert(thc_wait(event) && event[0]==8 && thc_event_age_ns()>=1000);
#endif
    char capture[256]; SDL_snprintf(capture,sizeof(capture),"/tmp/hide-native-atlas-%llu.bmp",(unsigned long long)SDL_GetTicksNS());
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE",argc>2?argv[2]:capture,true);
    scene();
    if (!thc_present()) { fprintf(stderr,"present: %s\n",thc_error()); return 1; }

    SDL_Surface *first=SDL_LoadBMP(argc>2?argv[2]:capture); assert(first);
    Uint8 red,green,blue,alpha;
    assert(SDL_ReadSurfacePixel(first,0,0,&red,&green,&blue,&alpha)); assert(red==255 && green==255 && blue==255);
    assert(SDL_ReadSurfacePixel(first,2,0,&red,&green,&blue,&alpha)); assert(red==0 && green==0 && blue==170);
    SDL_DestroySurface(first);
#ifndef HIDE_BASELINE
    uint64_t uploads,bytes,batches,warm,grid,gridBytes;
    thc_atlas_stats(&uploads,&bytes,&batches); thc_grid_stats(&grid,&gridBytes);
    assert(uploads>=3 && bytes<1000000);
    for (int i=0;i<3;++i) assert(thc_present());
    thc_atlas_stats(&warm,&bytes,&batches); assert(warm==uploads);
    uint64_t repeated,repeatedBytes; thc_grid_stats(&repeated,&repeatedBytes);
    assert(repeated==grid);
    scene(); assert(thc_present()); thc_atlas_stats(&warm,&bytes,&batches); assert(warm==uploads);
    if (strcmp(backend,"software")) { assert(grid==1 && gridBytes==80*25*32); assert(repeated==1); }
    printf("%s atlas misses=%llu uploaded bytes=%llu render draws=%llu grid=%llu/%llu bytes; warm atlas uploads=0, repeated grid uploads=0\n",
        thc_backend(),(unsigned long long)uploads,(unsigned long long)bytes,(unsigned long long)batches,(unsigned long long)grid,(unsigned long long)gridBytes);
    /* SDL invalidates the backbuffer at present. Production captures before
     * present; assertions read that completed capture, never a reused buffer. */
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE",capture,true);
    uint16_t half[16]; for (int y=0;y<16;++y) half[y]=0x00ff;
    assert(thc_begin()); thc_clip(0,1); thc_glyph(-1,0,2,16,half,0xffffff,0,0); assert(thc_present());
    SDL_Surface *image=SDL_LoadBMP(capture); assert(image);
    Uint8 r,g,b,a; assert(SDL_ReadSurfacePixel(image,0,0,&r,&g,&b,&a)); assert(r==255 && g==255 && b==255);
    assert(SDL_ReadSurfacePixel(image,16,0,&r,&g,&b,&a)); assert(r==0 && g==0 && b==0);
    SDL_DestroySurface(image);
    if (strcmp(backend,"software")) {
        /* Cross the bounded key table and initial atlas size in one frame.
         * An early cell must still sample its original rectangle after growth. */
        assert(thc_begin());
        uint16_t tile[16]; for (int y=0;y<16;++y) tile[y]=0xffff;
        thc_glyph(0,1,1,16,tile,0xffffff,0,0);
        for (int i=0;i<17000;++i) {
            tile[0]=0x8000|(i&0x7fff); tile[1]=(uint16_t)i;
            thc_glyph(1,1,1,16,tile,0xffffff,0,0);
        }
        assert(thc_present());
        image=SDL_LoadBMP(capture); assert(image);
        assert(SDL_ReadSurfacePixel(image,0,32,&r,&g,&b,&a)); assert(r==255 && g==255 && b==255);
        SDL_DestroySurface(image);
        puts("GPU atlas growth/key eviction preserves earlier cell rectangles");
    }
#endif
    /* Decorations belong to cells, not glyph tiles: toggling them must reuse
     * bitmap and shaped entries, including stretched and partially visible halves. */
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE",capture,true);
    decoration_scene(0); assert(thc_present());
    uint64_t decoration_before,decoration_after,decoration_bytes,decoration_draws;
    thc_atlas_stats(&decoration_before,&decoration_bytes,&decoration_draws);
    decoration_scene(24); assert(thc_present());
    thc_atlas_stats(&decoration_after,&decoration_bytes,&decoration_draws);
    assert(decoration_before==decoration_after);
    SDL_Surface *decorated=SDL_LoadBMP(capture); assert(decorated);
    pixel_is(decorated,0,30,255,255,255); pixel_is(decorated,0,14,0,0,170);
    pixel_is(decorated,32,14,255,255,255); pixel_is(decorated,32,30,0,0,170);
    for (int x=64;x<80;++x) { pixel_is(decorated,x,14,255,255,255); pixel_is(decorated,x,30,255,255,255); }
    for (int x=128;x<160;++x) pixel_is(decorated,x,30,255,255,255);
    pixel_is(decorated,192,30,255,255,255); pixel_is(decorated,208,30,0,0,170);
    pixel_is(decorated,224,30,255,255,255); pixel_is(decorated,240,30,0,0,170);
    for (int x=272;x<304;++x) pixel_is(decorated,x,14,255,255,255);
    pixel_is(decorated,64,10,0,0,170);
    SDL_DestroySurface(decorated);
    decoration_scene(24); thc_cursor_blink(0); thc_cursor(4,0); assert(thc_present());
    decorated=SDL_LoadBMP(capture); assert(decorated);
    pixel_is(decorated,64,30,0,0,0); pixel_is(decorated,64,14,255,255,255);
    SDL_DestroySurface(decorated);
    /* A short cell still displays both normalized line bands at pixel centers. */
    for (int i=0;i<8;++i) assert(thc_scale(-1));
    assert(thc_mode(8,80,25));
    SDL_FlushEvents(SDL_EVENT_FIRST,SDL_EVENT_LAST);
    SDL_zero(leave); leave.type=SDL_EVENT_WINDOW_MOUSE_LEAVE; assert(SDL_PushEvent(&leave));
    assert(thc_wait(event) && event[0]==12);
    decoration_scene(24); assert(thc_present());
    decorated=SDL_LoadBMP(capture); assert(decorated);
    pixel_is(decorated,0,7,255,255,255); pixel_is(decorated,0,3,0,0,170);
    pixel_is(decorated,16,3,255,255,255); pixel_is(decorated,16,7,0,0,170);
    pixel_is(decorated,96,7,255,255,255); pixel_is(decorated,104,7,0,0,170);
    SDL_DestroySurface(decorated);
    puts("Cell underline/strikethrough pixels, full/half glyph clips, cursor, 8-row mode and unchanged atlas identity passed");
    thc_close(); remove(capture); return 0;
}
