/* SPDX-License-Identifier: BSD-3-Clause */
/* Hidden real-GPU image resource/stencil/capture execution. */
#include "../cbits/window.h"
#include <SDL3/SDL.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __APPLE__
void thc_dock_close(void) {}
void thc_accessibility_close(void) {}
void thc_accessibility_geometry_changed(void) {}
int thc_accessibility_update(void *window,const char *json,size_t length) { (void)window;(void)json;(void)length;return 1; }
void thc_file_drag_close(void) {}
void thc_file_drag_arm(void *window,const char *path,double x,double y,double w,double h) { (void)window;(void)path;(void)x;(void)y;(void)w;(void)h; }
void thc_dock_raise(void *window) { (void)window; }
#endif
static const char *epoch="000000000000000000000000000000000000000000000001";
static const char *old="000000000000000000000000000000000000000000000000";
static const char *red="000000000000000000000000000000000000000000000002";
static const char *green="000000000000000000000000000000000000000000000003";
static const char *partial="000000000000000000000000000000000000000000000004";
static const char *colorful="000000000000000000000000000000000000000000000005";
static const char *capture="/private/tmp/hide-native-canvas.bmp";
static uint16_t mask[80*25];
static void cells(void) {
    uint16_t blank[16]={0},wide[16]; for (int y=0;y<16;++y) wide[y]=65535;
    assert(thc_begin());
    for (int y=0;y<25;++y) for (int x=0;x<80;++x) thc_glyph(x,y,1,8,blank,0xffffff,0x0000aa,0,0,1);
    thc_glyph(6,1,2,16,wide,0xffffff,0x0000aa,0,0,2);
}
static void scene(double shift) {
    assert(thc_canvas_scene(epoch,80,25,mask,80*25));
    assert(thc_canvas_surface(red,1,0,0,8,4,1+shift,1,6,2));
    assert(thc_canvas_surface(green,2,0,0,8,4,0,0,8,4));
    assert(thc_canvas_commit());
}
static void pixel(SDL_Surface *image,int x,int y,int r,int g,int b) {
    Uint8 actual[4]; assert(SDL_ReadSurfacePixel(image,x,y,&actual[0],&actual[1],&actual[2],&actual[3]));
    if (actual[0]!=r || actual[1]!=g || actual[2]!=b) {
        fprintf(stderr,"canvas pixel %d,%d: %u,%u,%u expected %d,%d,%d\n",x,y,actual[0],actual[1],actual[2],r,g,b); assert(0);
    }
}
int main(int argc,char **argv) {
    assert(argc==2);
#ifdef __APPLE__
    SDL_SetHint(SDL_HINT_MAC_BACKGROUND_APP,"1");
#endif
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE_EXIT","1",true);
    SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE",capture,true);
    assert(thc_open(argv[1],2,80,25,16));
    int count; SDL_Window **windows=SDL_GetWindows(&count); assert(count==1 && (SDL_GetWindowFlags(windows[0])&SDL_WINDOW_HIDDEN)); SDL_free(windows);
    assert(thc_canvas_reset(epoch));
    unsigned char rgba[16]; for (int i=0;i<4;++i) { rgba[i*4]=200;rgba[i*4+1]=100;rgba[i*4+2]=50;rgba[i*4+3]=255; }
    assert(!thc_canvas_begin(old,red,2,2,16));
    assert(!thc_canvas_begin(epoch,red,4096,4096,67108864));
    assert(thc_canvas_begin(epoch,red,2,2,16));
    assert(!thc_canvas_begin(epoch,red,2,2,16));
    assert(!thc_canvas_chunk(epoch,red,1,rgba,3));
    assert(thc_canvas_chunk(epoch,red,0,rgba,3));
    for (int y=0;y<4;++y) for (int x=0;x<8;++x) mask[y*80+x]=1;
    mask[80+2]=2; mask[80+3]=32769; mask[80+4]=0; mask[80+6]=0;
    cells(); scene(0); assert(thc_present());
    SDL_Surface *image=SDL_LoadBMP(capture); assert(image); pixel(image,16+8,32+16,0,0,170); SDL_DestroySurface(image);
    assert(thc_canvas_chunk(epoch,red,3,rgba+3,13));
    for (int i=0;i<4;++i) { rgba[i*4]=0;rgba[i*4+1]=151;rgba[i*4+2]=0;rgba[i*4+3]=128; }
    assert(thc_canvas_begin(epoch,green,2,2,16)); assert(thc_canvas_chunk(epoch,green,0,rgba,16));
    /* Completion redraws the retained scene without another scene packet. */
    assert(thc_present()); image=SDL_LoadBMP(capture); assert(image);
    pixel(image,16+8,32+16,200,100,50); pixel(image,2*16+8,32+16,0,75,0);
    pixel(image,3*16+8,32+16,100,50,25); pixel(image,4*16+8,32+16,0,0,170);
    pixel(image,6*16+8,32+16,255,255,255); pixel(image,7*16+8,32+16,0,0,0);
    pixel(image,8,16,0,0,0); SDL_DestroySurface(image);
    unsigned char colors[16]={255,0,0,255,0,255,0,128,0,0,255,255,151,255,0,128};
    assert(thc_canvas_begin(epoch,colorful,2,2,16));assert(thc_canvas_chunk(epoch,colorful,0,colors,16));
    uint16_t tiny[80*25]={0};tiny[0]=tiny[1]=tiny[81]=1;tiny[80]=32769;
    uint64_t uploads,bytes,masks,retained,draws,beforeUploads,beforeMasks,beforeDraws;
    thc_canvas_stats(&uploads,&bytes,&masks,&retained,&beforeDraws);
    cells();assert(thc_canvas_scene(epoch,80,25,tiny,80*25));
    assert(thc_canvas_surface(colorful,1,0,0,2,2,0,0,2,2));
    /* Retain occluded resources without issuing their shader passes. */
    assert(thc_canvas_surface(green,2,0,0,8,4,0,0,8,4));
    assert(thc_canvas_surface(red,64,0,0,8,4,0,0,8,4));
    assert(thc_canvas_commit());assert(thc_present());
    thc_canvas_stats(&uploads,&bytes,&masks,&retained,&draws);assert(draws==beforeDraws+1);
    image=SDL_LoadBMP(capture);assert(image);
    pixel(image,8,16,255,0,0);pixel(image,16+8,16,0,128,0);
    pixel(image,8,32+16,0,0,127);pixel(image,16+8,32+16,75,128,0);SDL_DestroySurface(image);
    assert(thc_canvas_release(epoch,colorful));cells();scene(0);assert(thc_present());
    thc_canvas_stats(&beforeUploads,&bytes,&beforeMasks,&retained,&draws); assert(bytes==48 && retained==32);
    for (int i=0;i<3;++i) { cells();scene(i); assert(thc_present()); }
    thc_canvas_stats(&uploads,&bytes,&masks,&retained,&draws); assert(uploads==beforeUploads && masks==beforeMasks);
    thc_crt_filter(1); cells();scene(0);assert(thc_present()); image=SDL_LoadBMP(capture);assert(image);
    pixel(image,16+8,32+31,200,100,50); SDL_DestroySurface(image); /* crisp pass follows CRT */
    assert(thc_canvas_begin(epoch,partial,2,2,16)); assert(thc_canvas_chunk(epoch,partial,0,rgba,1));
    assert(thc_canvas_release(epoch,partial)); assert(!thc_canvas_chunk(epoch,partial,1,rgba+1,15)); assert(thc_canvas_release(epoch,partial));
    thc_crt_filter(0);
    assert(thc_canvas_scene(epoch,80,25,NULL,0));assert(!thc_canvas_surface(red,1,0,0,8,4,0,0,8,4));assert(thc_canvas_commit());
    cells();assert(thc_present());image=SDL_LoadBMP(capture);assert(image);pixel(image,16+8,32+16,0,0,170);SDL_DestroySurface(image);
    thc_crt_filter(0);
    mask[0]=65; assert(!thc_canvas_scene(epoch,80,25,mask,80*25));mask[0]=1;
    assert(thc_canvas_scene(epoch,80,25,mask,80*25)); assert(thc_canvas_surface(red,1,0,0,1,1,0,0,1,1)); assert(!thc_canvas_commit());
    assert(thc_canvas_reset(old)); assert(!thc_canvas_release(epoch,red));
    thc_canvas_stats(&uploads,&bytes,&masks,&retained,&draws); assert(retained==0);
    thc_close(); remove(capture);
    puts("native canvas GPU: contiguous partial rows, ready gate, stencil overlap/halo/wide halves, straight alpha, crisp capture, retained transforms and epoch/release cleanup passed");
    return 0;
}
