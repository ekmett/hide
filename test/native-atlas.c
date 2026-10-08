/* SPDX-License-Identifier: BSD-3-Clause */
/* Hidden renderer execution, cell/atlas counters and full-origin clipping. */
#include "../cbits/window.h"
#include <SDL3/SDL.h>
#include <assert.h>
#include <stdio.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

/* Optional private allocation receipt over actual retained presentations, using
 * SDL's own allocator seam. Driver-internal and Haskell allocations are separate. */
static SDL_malloc_func allocation_malloc;
static SDL_calloc_func allocation_calloc;
static SDL_realloc_func allocation_realloc;
static SDL_free_func allocation_free;
static _Atomic uint64_t allocation_calls,allocation_bytes;
static void *SDLCALL measured_malloc(size_t bytes) {
    atomic_fetch_add(&allocation_calls,1); atomic_fetch_add(&allocation_bytes,bytes);
    return allocation_malloc(bytes);
}
static void *SDLCALL measured_calloc(size_t count,size_t bytes) {
    atomic_fetch_add(&allocation_calls,1); atomic_fetch_add(&allocation_bytes,count*bytes);
    return allocation_calloc(count,bytes);
}
static void *SDLCALL measured_realloc(void *pointer,size_t bytes) {
    atomic_fetch_add(&allocation_calls,1); atomic_fetch_add(&allocation_bytes,bytes);
    return allocation_realloc(pointer,bytes);
}
static void allocation_frames(const char *capture) {
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE","",true);
    thc_cursor_blink(0); thc_cursor(20,10);
    uint64_t base_calls=0,base_bytes=0;
    for (int active=0;active<2;++active) {
#ifndef HIDE_BASELINE
        thc_power_mode(active);
#endif
        for (int i=0;i<8;++i) assert(thc_present());
        uint64_t calls=atomic_load(&allocation_calls),bytes=atomic_load(&allocation_bytes);
        for (int i=0;i<32;++i) {
#ifndef HIDE_BASELINE
            if (active) thc_power_mode_burst();
#endif
            assert(thc_present());
        }
        calls=atomic_load(&allocation_calls)-calls; bytes=atomic_load(&allocation_bytes)-bytes;
        if (!active) { base_calls=calls; base_bytes=bytes; }
        else { assert(calls<=base_calls*2 && bytes<=base_bytes*2); }
        printf("private SDL allocation receipt: active=%d frames=32 calls=%llu bytes=%llu\n",active,
            (unsigned long long)calls,(unsigned long long)bytes);
    }
#ifndef HIDE_BASELINE
    thc_power_mode(0);
#endif
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE",capture,true);
}

#ifdef __APPLE__
void thc_dock_close(void) {}
void thc_accessibility_close(void) {}
void thc_accessibility_geometry_changed(void) {}
int thc_accessibility_update(void *window,const char *json,size_t length) { (void)window;(void)json;(void)length;return 1; }
void thc_file_drag_close(void) {}
void thc_file_drag_arm(void *window,const char *path,double x,double y,double width,double height) {
    (void)window;(void)path;(void)x;(void)y;(void)width;(void)height;
}
void thc_dock_raise(void *window) { (void)window; }
#endif

static void scene(void) {
    uint16_t glyph[16];
    for (int y=0;y<16;++y) glyph[y]=0x9000;
    assert(thc_begin());
    for (int y=0;y<25;++y) for (int x=0;x<80;++x)
        thc_glyph(x,y,1,8,glyph,0xffffff,0x0000aa,0,0,1);
    assert(thc_unicode(3,2,2,"👩🏽‍💻",0xffffff,0x0000aa,0,0,2));
    assert(thc_unicode(6,2,1,"é",0xffff55,0x0000aa,3,0,1));
    thc_glyph(10,2,2,8,glyph,0xffff55,0x0000aa,4,0,2);
}
static void decoration_scene(uint32_t lines) {
    uint16_t blank[16]={0};
    assert(thc_begin());
    for (int y=0;y<25;++y) for (int x=0;x<80;++x)
        thc_glyph(x,y,1,8,blank,0xffffff,0x0000aa,0,0,1);
    thc_glyph(0,0,1,8,blank,0xffffff,0x0000aa,lines&8,0,1);
    thc_glyph(2,0,1,8,blank,0xffffff,0x0000aa,lines&16,0,1);
    thc_glyph(4,0,1,8,blank,0xffffff,0x0000aa,lines,0,1);
    thc_glyph(8,0,2,8,blank,0xffffff,0x0000aa,4|lines,0,2);
    thc_clip(12,1); thc_glyph(11,0,2,8,blank,0xffffff,0x0000aa,4|lines,0,2);
    thc_clip(14,1); thc_glyph(14,0,2,8,blank,0xffffff,0x0000aa,4|lines,0,2);
    assert(thc_unicode(17,0,2," ",0xffffff,0x0000aa,7|lines,0,2));
}
static void pixel_is(SDL_Surface *image,int x,int y,Uint8 r,Uint8 g,Uint8 b);
#ifndef HIDE_BASELINE
static void power_mode_pixels(const char *capture) {
    uint16_t blank[16]={0};
    /* Colored particles must remain visible over light as well as dark paint.
     * Capture both ages before inspecting pixels, so CPU scanning cannot consume
     * the animation lifetime. Moving coverage distinguishes travel from tint/fade. */
    for (int light=0;light<2;++light) {
        assert(thc_begin());
        for (int y=0;y<25;++y) for (int x=0;x<80;++x)
            thc_glyph(x,y,1,8,blank,0xffffff,light?0xffffff:0x0000aa,0,0,1);
        thc_cursor_blink(0); thc_cursor(20,10); thc_power_mode(1);
        assert(thc_present());
        SDL_Surface *baseline=SDL_LoadBMP(capture); assert(baseline);
        uint64_t atlas_before,bytes,batches,grid_before;
        thc_atlas_stats(&atlas_before,&bytes,&batches); thc_grid_stats(&grid_before,&bytes);
        thc_power_mode_burst();
        SDL_FlushEvents(SDL_EVENT_FIRST,SDL_EVENT_LAST);
        int32_t event[6]; assert(thc_wait(event) && event[0]==17);
        SDL_Delay(65); assert(thc_present());
        SDL_Surface *early=SDL_LoadBMP(capture); assert(early);
        SDL_Delay(120); assert(thc_wait(event) && event[0]==17);
        assert(thc_present());
        SDL_Surface *late=SDL_LoadBMP(capture); assert(late);
        int visible=0,min_x=early->w,min_y=early->h,max_x=-1,max_y=-1;
        for (int y=0;y<baseline->h;++y) for (int x=0;x<baseline->w;++x) {
            Uint8 r,g,b,a,rr,gg,bb,aa;
            assert(SDL_ReadSurfacePixel(baseline,x,y,&r,&g,&b,&a));
            assert(SDL_ReadSurfacePixel(early,x,y,&rr,&gg,&bb,&aa));
            if (r!=rr || g!=gg || b!=bb) {
                assert(x>=20*16-128 && x<(21*16+128));
                assert(y>=10*32-128 && y<(11*32+128));
            }
            if (abs((int)r-rr)>=64 || abs((int)g-gg)>=64 || abs((int)b-bb)>=64) {
                ++visible;
                min_x=SDL_min(min_x,x); max_x=SDL_max(max_x,x);
                min_y=SDL_min(min_y,y); max_y=SDL_max(max_y,y);
            }
        }
        if (visible<64) fprintf(stderr,"Power Mode on %s paint: only %d contrasting pixels\n",light?"light":"dark",visible);
        assert(visible>=64);
        int travelled=0;
        for (int y=0;y<baseline->h;++y) for (int x=0;x<baseline->w;++x) {
            Uint8 r,g,b,a,rr,gg,bb,aa;
            assert(SDL_ReadSurfacePixel(baseline,x,y,&r,&g,&b,&a));
            assert(SDL_ReadSurfacePixel(late,x,y,&rr,&gg,&bb,&aa));
            if (r!=rr || g!=gg || b!=bb) {
                assert(x>=20*16-128 && x<(21*16+128));
                assert(y>=10*32-128 && y<(11*32+128));
            }
            if ((x<min_x-2 || x>max_x+2 || y<min_y-2 || y>max_y+2) &&
                (abs((int)r-rr)>=64 || abs((int)g-gg)>=64 || abs((int)b-bb)>=64)) ++travelled;
        }
        if (travelled<8) fprintf(stderr,"Power Mode on %s paint: only %d pixels travelled beyond the early burst\n",light?"light":"dark",travelled);
        assert(travelled>=8);
        SDL_DestroySurface(early); SDL_DestroySurface(late);
        thc_power_mode(0); assert(thc_present());
        SDL_Surface *disabled=SDL_LoadBMP(capture); assert(disabled);
        assert(baseline->pitch==disabled->pitch && baseline->h==disabled->h);
        assert(!memcmp(baseline->pixels,disabled->pixels,(size_t)baseline->pitch*baseline->h));
        SDL_DestroySurface(disabled);
        thc_power_mode(1); thc_power_mode_burst(); SDL_Delay(620); assert(thc_present());
        SDL_Surface *expired=SDL_LoadBMP(capture); assert(expired);
        assert(!memcmp(baseline->pixels,expired->pixels,(size_t)baseline->pitch*baseline->h));
        SDL_DestroySurface(expired); SDL_DestroySurface(baseline);
        uint64_t atlas_after,grid_after;
        thc_atlas_stats(&atlas_after,&bytes,&batches); thc_grid_stats(&grid_after,&bytes);
        assert(atlas_before==atlas_after && grid_before==grid_after);
        SDL_FlushEvents(SDL_EVENT_FIRST,SDL_EVENT_LAST);
        assert(thc_wait(event) && event[0]==0);
    }
    puts("Power Mode shader: contrasting sparks, travel, expiry, disable, retained atlas/grid and idle wake passed");
}
#endif
static void script_scene(int script) {
    uint16_t blank[16]={0},narrow[16],wide[16];
    for (int y=0;y<16;++y) { narrow[y]=0xff00; wide[y]=0xffff; }
    assert(thc_begin());
    for (int y=0;y<25;++y) for (int x=0;x<80;++x)
        thc_glyph(x,y,1,8,blank,0xffffff,0x0000aa,0,0,1);
    thc_glyph(0,0,1,8,narrow,0xffffff,0x0000aa,0,script,1);
    thc_glyph(2,0,script?1:2,16,wide,0xffffff,0x0000aa,0,script,2);
    thc_glyph(5,0,1,8,narrow,0xffffff,0x0000aa,24,script,1);
    thc_clip(9,1); thc_glyph(8,0,script?1:2,16,wide,0xffffff,0x0000aa,0,script,2);
    assert(thc_unicode(12,0,1,"é",0xffffff,0x0000aa,3,script,1));
    assert(thc_unicode(14,0,script?1:2,"界",0xffffff,0x0000aa,0,script,2));
    assert(thc_unicode(17,0,script?1:2,"👩🏽‍💻",0xffffff,0x0000aa,0,script,2));
}
static void script_pixels(const char *capture,int cell_width,int cell_height,int script) {
    SDL_Surface *image=SDL_LoadBMP(capture); assert(image);
    int upper=script==1?0:cell_height/2,other=script==1?cell_height/2:0;
    for (int y=0;y<cell_height;++y) for (int x=0;x<4*cell_width;++x) {
        bool ink=(y>=upper && y<upper+cell_height/2) && (x<cell_width/2 || (x>=2*cell_width && x<3*cell_width));
        pixel_is(image,x,y,ink?255:0,ink?255:0,ink?255:170);
    }
    /* Shaping/combining/emoji retain complete tiles, but no ink can occupy the
     * opposite band, narrow right quarter, or the following cell. */
    for (int x=12*cell_width;x<13*cell_width;++x) pixel_is(image,x,other,0,0,170);
    for (int x=12*cell_width+cell_width/2;x<14*cell_width;++x) pixel_is(image,x,upper,0,0,170);
    for (int x=14*cell_width;x<19*cell_width;++x) pixel_is(image,x,other,0,0,170);
    for (int x=15*cell_width;x<17*cell_width;++x) pixel_is(image,x,upper,0,0,170);
    for (int x=18*cell_width;x<19*cell_width;++x) pixel_is(image,x,upper,0,0,170);
    for (int glyph=0;glyph<3;++glyph) {
        int column=glyph==0?12:glyph==1?14:17,ink=0;
        for (int y=upper;y<upper+cell_height/2;++y) for (int x=column*cell_width;x<(column+1)*cell_width;++x) {
            Uint8 r,g,b,a; assert(SDL_ReadSurfacePixel(image,x,y,&r,&g,&b,&a));
            if (r!=0 || g!=0 || b!=170) ++ink;
        }
        assert(ink>0); /* Never accept an empty shaped/emoji script tile. */
    }
    pixel_is(image,5*cell_width+cell_width-1,cell_height-1,255,255,255);
    pixel_is(image,8*cell_width,upper,0,0,170); pixel_is(image,9*cell_width,upper,0,0,170);
    SDL_DestroySurface(image);
}
static uint32_t mouse_paint(uint32_t rgb) {
    const uint32_t palette[16]={0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,
        0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff};
    for (int i=0;i<16;++i) if (rgb==palette[i]) return palette[i^7];
    return rgb^0xaaaaaa;
}
static void script_pointer_pixels(const char *capture,int cell_width,int cell_height) {
    script_scene(2); assert(thc_present());
    SDL_Surface *reference=SDL_LoadBMP(capture); assert(reference);
    uint64_t before,after,bytes,draws; thc_atlas_stats(&before,&bytes,&draws);
    for (int phase=0;phase<3;++phase) {
        int target=phase?17:14;
        if (phase==1) {
            int count,logical_w,logical_h; SDL_Window **windows=SDL_GetWindows(&count); assert(windows && count==1);
            SDL_GetWindowSize(windows[0],&logical_w,&logical_h); SDL_free(windows);
            SDL_FlushEvents(SDL_EVENT_FIRST,SDL_EVENT_LAST);
            SDL_Event motion; SDL_zero(motion); motion.type=SDL_EVENT_MOUSE_MOTION;
            motion.motion.x=(target*cell_width+1.f)*logical_w/reference->w;
            motion.motion.y=1.f*logical_h/reference->h; assert(SDL_PushEvent(&motion));
            int32_t event[6]; assert(thc_wait(event) && event[0]==12 && event[1]==target && event[2]==0);
        }
        script_scene(2); if (phase!=1) { thc_cursor_blink(0); thc_cursor(target,0); } assert(thc_present());
        SDL_Surface *actual=SDL_LoadBMP(capture); assert(actual);
        for (int y=0;y<cell_height;++y) for (int x=target*cell_width;x<(target+1)*cell_width;++x) {
            Uint8 r,g,b,a; assert(SDL_ReadSurfacePixel(reference,x,y,&r,&g,&b,&a));
            uint32_t color=((uint32_t)r<<16)|((uint32_t)g<<8)|b;
            if (phase!=1 && y>=cell_height*14/16) color^=0xffffff;
            if (phase) color=mouse_paint(color);
            pixel_is(actual,x,y,color>>16,color>>8,color);
        }
        pixel_is(actual,(target+1)*cell_width,cell_height-1,0,0,170);
        SDL_DestroySurface(actual);
        thc_atlas_stats(&after,&bytes,&draws); assert(after==before);
    }
    SDL_FlushEvents(SDL_EVENT_FIRST,SDL_EVENT_LAST);
    SDL_Event leave; SDL_zero(leave); leave.type=SDL_EVENT_WINDOW_MOUSE_LEAVE; assert(SDL_PushEvent(&leave));
    int32_t event[6]; assert(thc_wait(event) && event[0]==12);
    SDL_DestroySurface(reference);
}
static void pixel_is(SDL_Surface *image,int x,int y,Uint8 r,Uint8 g,Uint8 b) {
    Uint8 actual_r,actual_g,actual_b,a;
    assert(SDL_ReadSurfacePixel(image,x,y,&actual_r,&actual_g,&actual_b,&a));
    if (actual_r!=r || actual_g!=g || actual_b!=b) fprintf(stderr,"pixel (%d,%d) in %dx%d: expected %u,%u,%u; actual %u,%u,%u\n",x,y,image->w,image->h,r,g,b,actual_r,actual_g,actual_b);
    assert(actual_r==r && actual_g==g && actual_b==b);
}
int main(int argc,char **argv) {
    const char *backend=argc>1?argv[1]:"software";
    bool measure=getenv("HIDE_TEST_ALLOCATIONS")!=NULL;
    if (measure) {
        SDL_GetOriginalMemoryFunctions(&allocation_malloc,&allocation_calloc,&allocation_realloc,&allocation_free);
        assert(SDL_SetMemoryFunctions(measured_malloc,measured_calloc,measured_realloc,allocation_free));
    }
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE_EXIT","1",true);
    if (!strcmp(backend,"software")) SDL_SetHint(SDL_HINT_VIDEO_DRIVER,"dummy");
#ifndef HIDE_BASELINE
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"HIDE_POWER_MODE","1",true);
#endif
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
    /* Empty baseline draws are no-ops; invalid scripted geometry never is. */
    assert(thc_unicode(0,0,0,"A",0xffffff,0,0,0,0));
    assert(!thc_unicode(0,0,0,"A",0xffffff,0,0,1,1));
    assert(!thc_unicode(0,0,0,"A",0xffffff,0,0,99,1));
    SDL_ClearError();
    char capture[256]; SDL_snprintf(capture,sizeof(capture),"/tmp/hide-native-atlas-%llu.bmp",(unsigned long long)SDL_GetTicksNS());
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE",argc>2?argv[2]:capture,true);
    scene();
    if (!thc_present()) { fprintf(stderr,"present: %s\n",thc_error()); return 1; }

    SDL_Surface *first=SDL_LoadBMP(argc>2?argv[2]:capture); assert(first);
    Uint8 red,green,blue,alpha;
    assert(SDL_ReadSurfacePixel(first,0,0,&red,&green,&blue,&alpha)); assert(red==255 && green==255 && blue==255);
    assert(SDL_ReadSurfacePixel(first,2,0,&red,&green,&blue,&alpha)); assert(red==0 && green==0 && blue==170);
    SDL_DestroySurface(first);
    if (measure) {
        allocation_frames(argc>2?argv[2]:capture);
        thc_close(); remove(capture); return 0;
    }
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
    if (strcmp(backend,"software")) power_mode_pixels(capture);
    uint16_t half[16]; for (int y=0;y<16;++y) half[y]=0x00ff;
    assert(thc_begin()); thc_clip(0,1); thc_glyph(-1,0,2,16,half,0xffffff,0,0,0,2); assert(thc_present());
    SDL_Surface *image=SDL_LoadBMP(capture); assert(image);
    Uint8 r,g,b,a; assert(SDL_ReadSurfacePixel(image,0,0,&r,&g,&b,&a)); assert(r==255 && g==255 && b==255);
    assert(SDL_ReadSurfacePixel(image,16,0,&r,&g,&b,&a)); assert(r==0 && g==0 && b==0);
    SDL_DestroySurface(image);
    if (strcmp(backend,"software")) {
        /* Cross the bounded key table and initial atlas size in one frame.
         * An early cell must still sample its original rectangle after growth. */
        assert(thc_begin());
        uint16_t tile[16]; for (int y=0;y<16;++y) tile[y]=0xffff;
        thc_glyph(0,1,1,16,tile,0xffffff,0,0,0,1);
        for (int i=0;i<17000;++i) {
            tile[0]=0x8000|(i&0x7fff); tile[1]=(uint16_t)i;
            thc_glyph(1,1,1,16,tile,0xffffff,0,0,0,1);
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
    uint64_t script_before,script_after;
    for (int pixelated=0;pixelated<=1;++pixelated) {
        thc_pixelate_unicode(pixelated);
        script_scene(0); assert(thc_present()); thc_atlas_stats(&script_before,&bytes,&batches);
        for (int script=1;script<=2;++script) {
            script_scene(script); assert(thc_present()); script_pixels(capture,16,32,script);
            thc_atlas_stats(&script_after,&bytes,&batches); assert(script_after==script_before);
        }
    }
    thc_pixelate_unicode(0);
    script_pointer_pixels(capture,16,32);
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
    script_scene(0); assert(thc_present());
    thc_atlas_stats(&script_before,&bytes,&batches);
    for (int script=1;script<=2;++script) {
        script_scene(script); assert(thc_present()); script_pixels(capture,8,8,script);
        thc_atlas_stats(&script_after,&bytes,&batches); assert(script_after==script_before);
    }
    script_pointer_pixels(capture,8,8);
    puts("Script ink bands/natural widths, mouse/cursor paint, normal-resolution atlas reuse and 8-row geometry passed");
    puts("Cell underline/strikethrough pixels, full/half glyph clips, cursor, 8-row mode and unchanged atlas identity passed");
    /* The first frame after an enlargement must fill the new backbuffer before
     * capture, including the far corner beyond the preceding drawable size. */
    const int resized[][2]={{126,29},{132,31},{80,25}};
    uint16_t stripe[16]; for (int y=0;y<16;++y) stripe[y]=0x9000;
    for (size_t i=0;i<sizeof(resized)/sizeof(*resized);++i) {
        int columns=resized[i][0],rows=resized[i][1];
        assert(thc_mode(16,columns,rows));
        assert(thc_begin());
        for (int y=0;y<rows;++y) for (int x=0;x<columns;++x)
            thc_glyph(x,y,1,8,stripe,0xffffff,0x0000aa,0,0,1);
        assert(thc_present());
        image=SDL_LoadBMP(capture); assert(image);
        assert(image->w==columns*8 && image->h==rows*16);
        pixel_is(image,(columns-1)*8,(rows-1)*16,255,255,255);
        pixel_is(image,(columns-1)*8+2,(rows-1)*16,0,0,170);
        SDL_DestroySurface(image);
    }
    puts("First enlarged/shrunk GPU frame captures the exact new drawable extent");
    thc_close(); remove(capture); return 0;
}
