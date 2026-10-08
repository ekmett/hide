#include "window.h"
#include "accessibility.h"
#include "unicode.h"
#include "shaders/cell.h"
#include "shaders/cell.generated.h"
#include <SDL3/SDL.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

static SDL_Window *window;
static SDL_GPUDevice *gpu;
static SDL_GPUShader *glyph_shader;
static SDL_GPURenderState *glyph_state;
static SDL_GPUBuffer *cell_buffer;
static SDL_GPUTransferBuffer *cell_transfer;
static struct HideGlyphCell *cell_grid;
static size_t cell_count,cell_capacity,cell_buffer_capacity;
static bool grid_ready;
static uint64_t grid_uploads,grid_bytes;
_Static_assert(sizeof(struct HideGlyphCell)==32,"Shader cell ABI");
static SDL_Renderer *renderer;
static SDL_Texture *texture, *vignette, *script_transform;
static SDL_PixelFormat script_transform_format;
static bool crt_filter;
static double scale;
static int cell_height;

static bool pixelate_unicode;
/* Semantic draw commands survive repeated present/blink; only atlas misses rasterize. */
typedef struct { int x,y,cells,width,clip_x,clip_width,script,natural; uint16_t bits[16]; char *text; uint32_t fg,bg,traits; bool pixelated; } GlyphCommand;
static GlyphCommand *commands;
static size_t command_count, command_capacity;
typedef struct { uint64_t hash; int x,y,w,h; uint32_t fg,bg,traits; int hover,cursor; bool pixelated; uint16_t bits[16]; char *text; } AtlasEntry;
#define INITIAL_ATLAS_SIZE 2048
static int atlas_size=INITIAL_ATLAS_SIZE;
static bool atlas_full;
#define ATLAS_SLOTS 4096
static AtlasEntry atlas[ATLAS_SLOTS];
static size_t atlas_keys;
static int atlas_x=1,atlas_y,atlas_row;
static void clear_atlas_entries(void) {
    for (size_t i=0;i<ATLAS_SLOTS;++i) free(atlas[i].text);
    memset(atlas,0,sizeof(atlas)); atlas_keys=0;
}

static bool draw_failed;
static int next_clip_x,next_clip_width=-1,draw_clip_x,draw_clip_width;
static uint64_t atlas_uploads,atlas_bytes,draw_batches,event_age;
static void clear_commands(void) {
    for (size_t i=0;i<command_count;++i) free(commands[i].text);
    command_count=0;
}
static GlyphCommand *command(void) {
    if (command_count==command_capacity) {
        size_t capacity=command_capacity?command_capacity*2:1024;
        GlyphCommand *next=realloc(commands,capacity*sizeof(*commands));
        if (!next) { SDL_SetError("Cannot allocate glyph commands"); draw_failed=true; return NULL; }
        commands=next; command_capacity=capacity;
    }
    GlyphCommand *next=&commands[command_count++]; memset(next,0,sizeof(*next)); next->clip_x=next_clip_x; next->clip_width=next_clip_width; next_clip_width=-1; return next;
}
static int cell_x(int x) { return (int)floor(x*8*scale); }
static int cell_y(int y) { return (int)floor(y*cell_height*scale); }


static int cols, rows, origin_x, origin_y, pixel_w, pixel_h, renderer_w, renderer_h;
static int mouse_x = -1, mouse_y = -1;
static char *input_text;
static char *clipboard_text;
static bool left_down;
static bool suppress_option_text;
static bool blink_cursor = true, cursor_present, cursor_drawn;
static int cursor_x = -1, cursor_y = -1;
static Uint64 cursor_epoch;
static Uint32 command_event, wake_event, dock_event;
static double wheel_remainder;
static void pointer(float x, float y, int32_t *event);

static void clear_pointer(void) {
    mouse_x = mouse_y = -1;
    SDL_ShowCursor();
}

const char *thc_error(void) { return SDL_GetError(); }
const char *thc_backend(void) { return gpu?SDL_GetGPUDeviceDriver(gpu):SDL_GetRendererName(renderer); }
const char *thc_text(void) { return input_text ? input_text : ""; }
const char *thc_clipboard(void) {
    SDL_free(clipboard_text);
    clipboard_text = SDL_GetClipboardText();
    return clipboard_text ? clipboard_text : "";
}
void thc_title(const char *s) { SDL_SetWindowTitle(window, s); }
void thc_set_clipboard(const char *s) { SDL_SetClipboardText(s); }

void thc_close(void) {
#ifdef __APPLE__
    thc_accessibility_close();
    thc_dock_close();
    thc_file_drag_close();
#endif
    clear_pointer();
    left_down = false;
    suppress_option_text = false;
    cursor_present = false; cursor_x = cursor_y = -1;
    SDL_StopTextInput(window);
    SDL_DestroyGPURenderState(glyph_state); glyph_state=NULL;
    if (gpu) {
        SDL_ReleaseGPUShader(gpu,glyph_shader); glyph_shader=NULL;
        SDL_ReleaseGPUBuffer(gpu,cell_buffer); cell_buffer=NULL;
        SDL_ReleaseGPUTransferBuffer(gpu,cell_transfer); cell_transfer=NULL;
    }
    free(cell_grid); cell_grid=NULL; cell_count=cell_capacity=cell_buffer_capacity=0;
    SDL_DestroyTexture(texture); texture = NULL;
    SDL_DestroyTexture(vignette); vignette = NULL;
    SDL_DestroyTexture(script_transform); script_transform=NULL; script_transform_format=SDL_PIXELFORMAT_UNKNOWN;
    SDL_DestroyRenderer(renderer); renderer = NULL; renderer_w=renderer_h=0;
    SDL_DestroyWindow(window); window = NULL;
    SDL_DestroyGPUDevice(gpu); gpu=NULL;
    clear_commands(); free(commands); commands=NULL; command_capacity=0;
    clear_atlas_entries(); atlas_x=1; atlas_y=atlas_row=0; atlas_size=INITIAL_ATLAS_SIZE;
    SDL_free(input_text); input_text = NULL;
    SDL_free(clipboard_text); clipboard_text = NULL;
    SDL_Quit();
}

static void geometry(void) {
    SDL_GetRenderOutputSize(renderer, &pixel_w, &pixel_h);
    cols = SDL_clamp(pixel_w / (8 * scale), 1, 512);
    rows = SDL_clamp(pixel_h / (cell_height * scale), 1, 256);
    origin_x = (pixel_w - cols * 8 * scale) / 2;
    origin_y = (pixel_h - rows * cell_height * scale) / 2;
#ifdef __APPLE__
    int ww,wh; SDL_GetWindowSize(window,&ww,&wh);
    static double previous[11];
    double current[]={cols,rows,origin_x,origin_y,pixel_w,pixel_h,ww,wh,scale,cell_height,(double)SDL_GetWindowID(window)};
    if (memcmp(previous,current,sizeof(current))) {
        memcpy(previous,current,sizeof(current));
        thc_accessibility_geometry_changed();
    }
#endif
}

#ifdef __APPLE__
int thc_accessibility_cell_rect(int x,int y,int width,int height,double rectangle[4]) {
    if (!window || !rectangle || pixel_w<=0 || pixel_h<=0 || x<0 || y<0 || width<=0 || height<=0 || x>=cols || y>=rows || width>cols-x || height>rows-y) return 0;
    int ww,wh; SDL_GetWindowSize(window,&ww,&wh);
    double sx=(double)ww/pixel_w,sy=(double)wh/pixel_h;
    rectangle[0]=(origin_x+cell_x(x))*sx; rectangle[1]=(origin_y+cell_y(y))*sy;
    rectangle[2]=(cell_x(x+width)-cell_x(x))*sx; rectangle[3]=(cell_y(y+height)-cell_y(y))*sy;
    return 1;
}
#endif
int thc_accessibility(const char *json,size_t length) {
#ifdef __APPLE__
    if (length && window) geometry();
    void *native=window?SDL_GetPointerProperty(SDL_GetWindowProperties(window),SDL_PROP_WINDOW_COCOA_WINDOW_POINTER,NULL):NULL;
    if (!thc_accessibility_update(native,json,length)) return SDL_SetError("Invalid or unavailable sidebar accessibility metadata");
#else
    (void)json; (void)length;
#endif
    return 1;
}

static void refresh_pointer(void) {
    if (SDL_GetMouseFocus() == window) {
        float x, y; int32_t ignored[6];
        SDL_GetMouseState(&x, &y);
        pointer(x, y, ignored);
    } else clear_pointer();
}

int thc_open(const char *backend, double requested_scale, int requested_cols, int requested_rows, int height) {
    if (!isfinite(requested_scale) || (requested_scale != 0 && (requested_scale < 1 || requested_scale > 8)))
        return SDL_SetError("Tile scale must be between 1 and 8");
    SDL_SetAppMetadata("Haskell", "0.1.0.0", NULL);
    if (!SDL_Init(SDL_INIT_VIDEO)) return 0;
    crt_filter = false;
    blink_cursor = true; cursor_epoch = SDL_GetTicks();
    command_event = SDL_RegisterEvents(3);
    wake_event = command_event + 1;
    dock_event = command_event + 2;
    wheel_remainder = 0;
    /* Documentation captures use the real Metal renderer without showing a window. */
    const char *capture_exit = SDL_getenv("THC_EDIT_CAPTURE_EXIT");
    SDL_WindowFlags flags = SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY;
    if (capture_exit && strcmp(capture_exit, "1") == 0) flags |= SDL_WINDOW_HIDDEN;
    window = SDL_CreateWindow("Haskell", 1280, 800, flags);
    if (!window) return 0;
    if (strcmp(backend,"software")) {
        gpu=SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV|SDL_GPU_SHADERFORMAT_MSL,false,backend);
        if (!gpu) return 0;
        renderer=SDL_CreateGPURenderer(gpu,window);
        bool metal=!strcmp(SDL_GetGPUDeviceDriver(gpu),"metal");
        SDL_GPUShaderCreateInfo shader={0};
        shader.code=metal?hide_cell_msl:hide_cell_spv;
        shader.code_size=metal?sizeof(hide_cell_msl):sizeof(hide_cell_spv);
        shader.entrypoint=metal?"main0":"main";
        shader.format=metal?SDL_GPU_SHADERFORMAT_MSL:SDL_GPU_SHADERFORMAT_SPIRV;
        shader.stage=SDL_GPU_SHADERSTAGE_FRAGMENT; shader.num_samplers=1; shader.num_storage_buffers=1; shader.num_uniform_buffers=1;
        glyph_shader=SDL_CreateGPUShader(gpu,&shader);
        if (!glyph_shader) return 0;
    } else renderer=SDL_CreateRenderer(window,backend);
    if (!renderer || !SDL_GetRenderOutputSize(renderer,&renderer_w,&renderer_h)) return 0;
    SDL_SetRenderVSync(renderer, 1);
    int pw, ph, ww, wh;
    SDL_GetWindowSizeInPixels(window, &pw, &ph);
    SDL_GetWindowSize(window, &ww, &wh);
    double density = (double)pw / SDL_max(1, ww);
    scale = requested_scale ? round(requested_scale * 8) / 8 : SDL_max(1, (int)lround(2 * density));
    if (!thc_mode(height, requested_cols, requested_rows)) return 0;
    return SDL_StartTextInput(window);
}

int thc_mode(int height, int requested_cols, int requested_rows) {
    if (height != 8 && height != 16) return SDL_SetError("Cell height must be 8 or 16");
    int pw, ph, ww, wh, min_w, min_h;
    SDL_GetWindowSizeInPixels(window, &pw, &ph);
    SDL_GetWindowSize(window, &ww, &wh);
    SDL_GetWindowMinimumSize(window, &min_w, &min_h);
    double density = (double)pw / SDL_max(1, ww);
    /* Lower the old minimum before switching a small custom-size window. */
    /* X11/Wayland resize requests can complete after SetWindowSize returns. */
    if (!SDL_SetWindowMinimumSize(window, (int)ceil(40 * 8 * scale / density), (int)ceil(12 * height * scale / density)) ||
        !SDL_SetWindowSize(window, (int)ceil(requested_cols * 8 * scale / density), (int)ceil(requested_rows * height * scale / density)) ||
        !SDL_SyncWindow(window)) {
        char error[1024]; SDL_strlcpy(error, SDL_GetError(), sizeof(error));
        SDL_SetWindowMinimumSize(window, min_w, min_h);
        SDL_SetWindowSize(window, ww, wh);
        SDL_SyncWindow(window);
        return SDL_SetError("%s", error);
    }
    cell_height = height;
    geometry();
    refresh_pointer();
    return 1;
}

int thc_scale(int direction) {
    geometry();
    double previous = scale;
    int width = cols, height = rows;
    int pw, ph, ww, wh;
    SDL_GetWindowSizeInPixels(window, &pw, &ph);
    SDL_GetWindowSize(window, &ww, &wh);
    int default_scale = (int)lround(2.0 * pw / SDL_max(1, ww));
    scale = SDL_clamp(direction == 0 ? default_scale : scale + (direction > 0 ? 0.125 : -0.125), 1, 8);
    if (scale == previous || thc_mode(cell_height, width, height)) return 1;
    scale = previous;
    geometry();
    return 0;
}

void thc_file_drag_ended(void) {
    left_down=false;
    SDL_CaptureMouse(false);
    float x,y; SDL_GetMouseState(&x,&y);
    SDL_Event event={0}; event.type=SDL_EVENT_MOUSE_BUTTON_UP;
    event.button.windowID=SDL_GetWindowID(window); event.button.button=SDL_BUTTON_LEFT;
    event.button.x=x; event.button.y=y;
    SDL_PushEvent(&event);
}
void thc_cancel_file_drag(void) {
#ifdef __APPLE__
    thc_file_drag_close();
#endif
}
int thc_arm_file_drag(const char *path, int x, int y, int width, int height) {
#ifdef __APPLE__
    if (x<0 || y<0 || width<=0 || height<=0 || x>=cols || y>=rows || width>cols-x || height>rows-y) return SDL_SetError("The exported file row is no longer visible");
    int ww,wh;
    SDL_GetWindowSize(window,&ww,&wh);
    geometry();
    double sx=(double)ww/pixel_w,sy=(double)wh/pixel_h;
    void *native=SDL_GetPointerProperty(SDL_GetWindowProperties(window),SDL_PROP_WINDOW_COCOA_WINDOW_POINTER,NULL);
    if (!native) return SDL_SetError("Native file drag is unavailable");
    thc_file_drag_arm(native,path,(origin_x+cell_x(x))*sx,(origin_y+cell_y(y))*sy,
                      (cell_x(x+width)-cell_x(x))*sx,(cell_y(y+height)-cell_y(y))*sy);
    return 1;
#else
    (void)path;(void)x;(void)y;(void)width;(void)height;
    return SDL_SetError("Native file drag is unavailable on this frontend");
#endif
}
void thc_size(int *w, int *h) { geometry(); *w = cols; *h = rows; }
int thc_begin(void) {
    geometry();
    /* SDL's GPU renderer recreates its backbuffer only at present, after
     * acquiring the resized swapchain. Refresh that boundary before authoring
     * the first new-size frame; otherwise drawing/capture uses the old extent.
     * Ordinary frames still prepare and present their grid exactly once. */
    if (gpu && (pixel_w!=renderer_w || pixel_h!=renderer_h)) {
        if (!SDL_RenderPresent(renderer)) return 0;
        renderer_w=pixel_w; renderer_h=pixel_h;
    }
    cursor_present=false; clear_commands(); draw_failed=false; grid_ready=false;
    if (!texture) {
        /* Tile uploads never lock a streaming CPU mirror of the whole atlas. */
        texture=SDL_CreateTexture(renderer,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_STATIC,atlas_size,atlas_size);
        if (!texture || !SDL_SetTextureScaleMode(texture,SDL_SCALEMODE_NEAREST) ||
            !SDL_SetTextureBlendMode(texture,SDL_BLENDMODE_BLEND)) return 0;
        uint32_t white=0xffffffff; SDL_Rect pixel={0,0,1,1};
        if (!SDL_UpdateTexture(texture,&pixel,&white,4)) return 0;
    }
    return 1;
}
void thc_clip(int visible_x,int clip_cells) { next_clip_x=visible_x; next_clip_width=clip_cells; }
void thc_glyph(int x,int y,int cells,int glyph_width,const uint16_t *bits,uint32_t fg,uint32_t bg,uint32_t traits,int script,int natural_cells) {
    if (y<0 || y>=rows) return;
    if (script && ((script!=1 && script!=2) || cells!=1 || (natural_cells!=1 && natural_cells!=2))) { SDL_SetError("Invalid script glyph geometry"); draw_failed=true; return; }
    GlyphCommand *c=command(); if (!c) return;
    c->script=script; c->natural=natural_cells;
    c->x=x; c->y=y; c->cells=cells; c->width=glyph_width; c->fg=fg; c->bg=bg; c->traits=traits;
    memcpy(c->bits,bits,sizeof(c->bits));
}
void thc_pixelate_unicode(int enabled) { pixelate_unicode=enabled!=0; }
int thc_unicode(int x,int y,int cells,const char *text,uint32_t fg,uint32_t bg,uint32_t traits,int script,int natural_cells) {
    if (y<0 || y>=rows) return 1;
    if (script && ((script!=1 && script!=2) || cells!=1 || (natural_cells!=1 && natural_cells!=2))) return SDL_SetError("Invalid script glyph geometry");
    if (cells<1) return 1;
    GlyphCommand *c=command(); if (!c) return 0;
    c->script=script; c->natural=natural_cells;
    c->x=x; c->y=y; c->cells=cells; c->fg=fg; c->bg=bg; c->traits=traits; c->pixelated=pixelate_unicode;
    c->text=strdup(text); return c->text!=NULL;
}
static bool cursor_phase(void) {
    return !blink_cursor || ((SDL_GetTicks() - cursor_epoch) / 500) % 2 == 0;
}
void thc_cursor_blink(int enabled) {
    if (blink_cursor != (enabled != 0)) cursor_epoch = SDL_GetTicks();
    blink_cursor = enabled != 0;
}
void thc_cursor(int x,int y) {
    if (x<0 || x>=cols || y<0 || y>=rows) return;
    if (cursor_x!=x || cursor_y!=y) cursor_epoch=SDL_GetTicks();
    cursor_x=x; cursor_y=y; cursor_present=true;
}
static uint32_t mouse_color(uint32_t pixel) {
    /* DOS text mouse: screen mask FFFF, cursor mask 7700. Keep glyph/intensity. */
    static const uint32_t palette[16] = {
        0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,
        0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff
    };
    uint32_t rgb = pixel & 0x00ffffff;
    for (int i = 0; i < 16; ++i)
        if (rgb == palette[i]) return (pixel & 0xff000000) | palette[i ^ 7];
    return pixel ^ 0x00aaaaaa;
}
void thc_crt_filter(int enabled) { crt_filter = enabled != 0; }

static bool draw_crt(const SDL_FRect *target) {
    if (!vignette) {
        SDL_Surface *surface = SDL_CreateSurface(64, 64, SDL_PIXELFORMAT_ARGB8888);
        if (!surface) return false;
        for (int y = 0; y < 64; ++y) for (int x = 0; x < 64; ++x) {
            float nx = (x - 31.5f) / 31.5f, ny = (y - 31.5f) / 31.5f;
            float radius = (nx * nx + ny * ny) / 2;
            ((Uint32 *)((Uint8 *)surface->pixels + y * surface->pitch))[x] =
                (Uint32)(100 * radius * radius) << 24;
        }
        vignette = SDL_CreateTextureFromSurface(renderer, surface);
        SDL_DestroySurface(surface);
        if (!vignette || !SDL_SetTextureBlendMode(vignette, SDL_BLENDMODE_BLEND) ||
            !SDL_SetTextureScaleMode(vignette, SDL_SCALEMODE_LINEAR)) return false;
    }
    /* Follow all 16 bitmap rows even in the compressed 80x50 mode. Do not
     * darken single-pixel strokes when there is no room between glyph rows. */
    double glyph_pitch = cell_height * scale / 16;
    if (glyph_pitch >= 2) {
        SDL_FRect lines[4096];
        int count = rows * 16;
        for (int y = 0; y < count; ++y)
            lines[y] = (SDL_FRect){target->x, target->y + (float)floor((y + 1) * glyph_pitch) - 1, target->w, 1};
        if (!SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND) ||
            !SDL_SetRenderDrawColor(renderer, 0, 0, 0, 24) ||
            !SDL_RenderFillRects(renderer, lines, count)) return false;
    }
    return SDL_RenderTexture(renderer, vignette, NULL, target);
}
static bool quad(float x,float y,float w,float h,float u,float v,float uw,float vh,uint32_t color) {
    if (draw_clip_width>=0) {
        float lo=origin_x+cell_x(draw_clip_x),hi=origin_x+cell_x(draw_clip_x+draw_clip_width);
        float start=SDL_max(x,lo),end=SDL_min(x+w,hi);
        if (start>=end) return true;
        u+=(start-x)*uw/w; uw*=(end-start)/w; x=start; w=end-start;
    }
    /* The software test renderer's triangle sampler rounds atlas UV edges;
     * explicit source rectangles preserve the exact same clipped texels. */
    SDL_FRect source={floorf(u*atlas_size),floorf(v*atlas_size),SDL_max(1.f,uw*atlas_size),SDL_max(1.f,vh*atlas_size)};
    SDL_FRect destination={x,y,w,h};
    ++draw_batches;
    return SDL_SetTextureColorMod(texture,(color>>16)&255,(color>>8)&255,color&255) && SDL_RenderTexture(renderer,texture,&source,&destination);
}

static uint64_t hash_bytes(uint64_t hash,const void *bytes,size_t count) {
    const unsigned char *p=bytes;
    for (size_t i=0;i<count;++i) hash=(hash^p[i])*1099511628211ULL;
    return hash;
}
/* Keep growth in the renderer's command stream: SDL_UpdateTexture records
 * uploads there, and SDL_FlushRenderer does not submit that GPU buffer. An
 * independent GPU copy could therefore run before the source uploads. */
static bool grow_atlas(void) {
    if (atlas_size>=8192) { atlas_full=true; return false; }
    int size=atlas_size*2;
    SDL_Texture *next=SDL_CreateTexture(renderer,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_TARGET,size,size);
    if (!next) return false;
    SDL_Texture *previous=SDL_GetRenderTarget(renderer);
    SDL_FRect area={0,0,(float)atlas_size,(float)atlas_size};
    bool ok=SDL_SetTextureScaleMode(next,SDL_SCALEMODE_NEAREST) &&
        SDL_SetTextureBlendMode(next,SDL_BLENDMODE_BLEND) &&
        SDL_SetTextureBlendMode(texture,SDL_BLENDMODE_NONE) &&
        SDL_SetRenderTarget(renderer,next) && SDL_SetRenderClipRect(renderer,NULL) &&
        SDL_RenderTexture(renderer,texture,NULL,&area) && SDL_FlushRenderer(renderer);
    if (!SDL_SetRenderTarget(renderer,previous)) ok=false;
    if (!ok) { SDL_DestroyTexture(next); return false; }
    SDL_DestroyTexture(texture); texture=next; atlas_size=size; return true;
}
static AtlasEntry *atlas_entry(const GlyphCommand *c,int hover,int cursor) {
    int raster_cells=c->script?c->natural:c->cells;
    int w=c->text?(c->pixelated?8*raster_cells:cell_x(c->x+raster_cells)-cell_x(c->x)):c->width;
    int h=c->text && !c->pixelated?cell_y(c->y+1)-cell_y(c->y):16;
    /* Line decorations are cell paint, never atlas glyph identity. */
    uint32_t glyph_traits=c->traits&7;
    uint64_t hash=hash_bytes(14695981039346656037ULL,&w,sizeof(w));
    hash=hash_bytes(hash,&h,sizeof(h)); hash=hash_bytes(hash,&glyph_traits,sizeof(glyph_traits));
    if (c->text) {
        hash=hash_bytes(hash,c->text,strlen(c->text)); hash=hash_bytes(hash,&c->fg,sizeof(c->fg));
        hash=hash_bytes(hash,&c->pixelated,sizeof(c->pixelated)); hash=hash_bytes(hash,&hover,sizeof(hover)); hash=hash_bytes(hash,&cursor,sizeof(cursor));
        if (hover>=0 || cursor>=0) hash=hash_bytes(hash,&c->bg,sizeof(c->bg));
    } else hash=hash_bytes(hash,c->bits,sizeof(c->bits));
    if (!hash) hash=1;
    size_t slot=hash%ATLAS_SLOTS;
    for (size_t n=0;atlas[slot].hash && n<ATLAS_SLOTS;++n,slot=(slot+1)%ATLAS_SLOTS)
        if (atlas[slot].hash==hash && atlas[slot].w==w && atlas[slot].h==h && atlas[slot].traits==glyph_traits &&
            (c->text?atlas[slot].text && !strcmp(atlas[slot].text,c->text) && atlas[slot].fg==c->fg &&
                atlas[slot].pixelated==c->pixelated && atlas[slot].hover==hover && atlas[slot].cursor==cursor &&
                ((hover<0 && cursor<0) || atlas[slot].bg==c->bg):!atlas[slot].text && !memcmp(atlas[slot].bits,c->bits,sizeof(c->bits)))) return &atlas[slot];
    if (w<1 || h<1 || w>8192 || h>8192) { SDL_SetError("Glyph exceeds atlas dimensions"); return NULL; }
    if (gpu) {
        while (w>atlas_size || h>atlas_size) if (!grow_atlas()) return NULL;
        if (atlas_x+w>atlas_size) { atlas_x=0; atlas_y+=atlas_row; atlas_row=0; }
        while (atlas_y+h>atlas_size) if (!grow_atlas()) return NULL;
        /* Forgetting keys never reuses pixels: prepared cells own rectangles.
         * Retire the bounded key table before it becomes a full-table probe. */
        if (atlas_keys>=ATLAS_SLOTS*3/4) { clear_atlas_entries(); slot=hash%ATLAS_SLOTS; }
    } else {
        if (w>atlas_size || h>atlas_size) { SDL_SetError("Glyph exceeds atlas dimensions"); return NULL; }
        if (atlas_x+w>atlas_size) { atlas_x=0; atlas_y+=atlas_row; atlas_row=0; }
        if (atlas_y+h>atlas_size || atlas[slot].hash) {
            if (!SDL_FlushRenderer(renderer)) return NULL;
            clear_atlas_entries(); atlas_x=1; atlas_y=atlas_row=0; slot=hash%ATLAS_SLOTS;
        }
    }
    uint32_t *pixels=calloc((size_t)w*h,sizeof(*pixels));
    if (!pixels) { SDL_SetError("Cannot allocate glyph tile"); return NULL; }
    bool ok=true;
    if (c->text) {
        ok=c->pixelated?thc_unicode_pixelated(c->text,w,h,c->fg,glyph_traits,pixels):thc_unicode_bitmap(c->text,w,h,c->fg,glyph_traits,pixels);
        if (ok && (hover>=0 || cursor>=0)) for (int y=0;y<h;++y) for (int x=0;x<w;++x) {
            uint32_t ink=pixels[y*w+x]; unsigned alpha=ink>>24,inverse=255-alpha;
            uint32_t rgb=((((ink>>16)&255)+((c->bg>>16)&255)*inverse/255)<<16)|
                ((((ink>>8)&255)+((c->bg>>8)&255)*inverse/255)<<8)|((ink&255)+(c->bg&255)*inverse/255);
            int cell=x*c->cells/w;
            if (cell==cursor && y>=h*14/16) rgb^=0xffffff;
            if (cell==hover) rgb=mouse_color(rgb);
            pixels[y*w+x]=0xff000000|rgb;
        }
    } else for (int y=0;y<16;++y) for (int x=0;x<w;++x) {
        int source=x-((c->traits&2)?(15-y)/4:0);
        bool ink=source>=0 && source<16 && (c->bits[y]&(0x8000u>>source));
        if ((c->traits&1) && source>0 && source<=16) ink=ink || (c->bits[y]&(0x8000u>>(source-1)));
        pixels[y*w+x]=ink?0xffffffff:0;
    }
    /* Portable SDL geometry blending uses straight alpha; shaping remains premultiplied. */
    if (c->text && ok && hover<0 && cursor<0) for (int i=0;i<w*h;++i) {
        unsigned a=pixels[i]>>24;
        if (a && a<255) pixels[i]=(a<<24)|((SDL_min(255,((pixels[i]>>16&255)*255+a/2)/a))<<16)|
            ((SDL_min(255,((pixels[i]>>8&255)*255+a/2)/a))<<8)|SDL_min(255,((pixels[i]&255)*255+a/2)/a);
    }
    SDL_Rect region={atlas_x,atlas_y,w,h};
    if (ok) ok=SDL_UpdateTexture(texture,&region,pixels,w*4);
    free(pixels); if (!ok) return NULL;
    ++atlas_uploads; atlas_bytes+=(uint64_t)w*h*4;
    AtlasEntry cached={0}; cached.hash=hash; cached.x=atlas_x; cached.y=atlas_y; cached.w=w; cached.h=h;
    cached.fg=c->fg; cached.bg=c->bg; cached.traits=glyph_traits; cached.hover=hover; cached.cursor=cursor; cached.pixelated=c->pixelated;
    memcpy(cached.bits,c->bits,sizeof(cached.bits));
    if (c->text) { cached.text=strdup(c->text); if (!cached.text) return NULL; }
    atlas[slot]=cached; ++atlas_keys; atlas_x+=w; atlas_row=SDL_max(atlas_row,h);
    return &atlas[slot];
}
/* Software fallback mirrors the shader's normalized font-row decorations.
 * These strips use the same clip and paint transforms as their owning cells. */
static bool draw_decorations(const GlyphCommand *c,int count,float y,float height,int hover,int cursor) {
    for (int line=0;line<2;++line) {
        if (!(c->traits&(line?16:8))) continue;
        int row=line?7:15;
        float top=floorf(height*row/16),bottom=ceilf(height*(row+1)/16);
        for (int cell=0;cell<count;++cell) {
            uint32_t fg=c->fg;
            if (cell==cursor && row>=14) fg^=0xffffff;
            if (cell==hover) fg=mouse_color(fg);
            float x=origin_x+cell_x(c->x+cell),width=cell_x(c->x+cell+1)-cell_x(c->x+cell);
            if (!quad(x,y+top,width,SDL_max(1.f,bottom-top),0.5f/atlas_size,0.5f/atlas_size,0,0,fg)) return false;
        }
    }
    return true;
}
/* The software renderer has no final-cell shader. Only a targeted script cell
 * needs this bounded readback: shrinking a pre-inverted normal glyph would move
 * its cursor band. Keep normal atlas identity and transform composed cell paint. */
static bool transform_script_cell(const GlyphCommand *c,int hover,int cursor) {
    if (gpu) return true; /* GPU kernels already transform final cell paint. */
    if (hover<0 && cursor<0) return true;
    if (c->x<0 || c->x>=cols || (c->clip_width>=0 && (c->x<c->clip_x || c->x>=c->clip_x+c->clip_width))) return true;
    SDL_Rect area={origin_x+cell_x(c->x),origin_y+cell_y(c->y),cell_x(c->x+1)-cell_x(c->x),cell_y(c->y+1)-cell_y(c->y)};
    SDL_Surface *painted=SDL_RenderReadPixels(renderer,&area); if (!painted) return false;
    bool ok=true;
    for (int y=0;ok && y<painted->h;++y) for (int x=0;ok && x<painted->w;++x) {
        Uint8 r=0,g=0,b=0,a=0;
        ok=SDL_ReadSurfacePixel(painted,x,y,&r,&g,&b,&a);
        uint32_t color=((uint32_t)r<<16)|((uint32_t)g<<8)|b;
        if (cursor>=0 && y>=area.h*14/16) color^=0xffffff;
        if (hover>=0) color=mouse_color(color);
        if (ok) ok=SDL_WriteSurfacePixel(painted,x,y,color>>16,color>>8,color,255);
    }
    float w=0,h=0;
    if (script_transform) SDL_GetTextureSize(script_transform,&w,&h);
    if (!script_transform || w!=painted->w || h!=painted->h || script_transform_format!=painted->format) {
        SDL_DestroyTexture(script_transform);
        script_transform=SDL_CreateTexture(renderer,painted->format,SDL_TEXTUREACCESS_STREAMING,painted->w,painted->h);
        script_transform_format=painted->format;
        if (script_transform) ok=ok && SDL_SetTextureScaleMode(script_transform,SDL_SCALEMODE_NEAREST) && SDL_SetTextureBlendMode(script_transform,SDL_BLENDMODE_NONE);
    }
    SDL_FRect target={(float)area.x,(float)area.y,(float)area.w,(float)area.h};
    ok=ok && script_transform && SDL_UpdateTexture(script_transform,NULL,painted->pixels,painted->pitch) && SDL_RenderTexture(renderer,script_transform,NULL,&target);
    SDL_DestroySurface(painted);
    return ok;
}
static bool draw_command(const GlyphCommand *c) {
    draw_clip_x=c->clip_x; draw_clip_width=c->clip_width;
    int count=c->script?1:SDL_max(c->cells,(c->width+7)/8);
    int hover=!left_down && mouse_y==c->y && mouse_x>=c->x && mouse_x<c->x+count?mouse_x-c->x:-1;
    int cursor=cursor_present && cursor_drawn && cursor_y==c->y && cursor_x>=c->x && cursor_x<c->x+count?cursor_x-c->x:-1;
    AtlasEntry *entry=atlas_entry(c,c->script?-1:hover,c->script?-1:cursor); if (!entry) return false;
    float y=origin_y+cell_y(c->y),height=cell_y(c->y+1)-cell_y(c->y);
    if (c->script) {
        float x=origin_x+cell_x(c->x),width=cell_x(c->x+1)-cell_x(c->x);
        if (!quad(x,y,width,height,0.5f/atlas_size,0.5f/atlas_size,0,0,c->bg) ||
            !quad(x,y+(c->script==2?height/2:0),width*c->natural/2,height/2,
                (entry->x+0.5f)/atlas_size,(entry->y+0.5f)/atlas_size,entry->w/(float)atlas_size,entry->h/(float)atlas_size,c->text?0xffffff:c->fg) ||
            !draw_decorations(c,1,y,height,-1,-1)) return false;
        return transform_script_cell(c,hover,cursor);
    }
    if (c->text) {
        float x=origin_x+cell_x(c->x),width=cell_x(c->x+c->cells)-cell_x(c->x);
        if (!quad(x,y,width,height,0.5f/atlas_size,0.5f/atlas_size,0,0,c->bg)) return false;
        if (!quad(x,y,width,height,(entry->x+0.5f)/atlas_size,(entry->y+0.5f)/atlas_size,entry->w/(float)atlas_size,entry->h/(float)atlas_size,0xffffff)) return false;
        return draw_decorations(c,count,y,height,hover,cursor);
    }
    for (int cell=0;cell<count;++cell) for (int part=0;part<2;++part) {
        float x=origin_x+cell_x(c->x+cell),width=cell_x(c->x+cell+1)-cell_x(c->x+cell);
        float py=y+(part?height*14/16:0),ph=height*(part?2:14)/16;
        uint32_t fg=c->fg,bg=c->bg;
        if (cell==cursor && part) { fg^=0xffffff; bg^=0xffffff; }
        if (cell==hover) { fg=mouse_color(fg); bg=mouse_color(bg); }
        if (c->cells && !quad(x,py,width,ph,0.5f/atlas_size,0.5f/atlas_size,0,0,bg)) return false;
        float source=cell*8.f/((c->traits&4)?2:1),sourceWidth=8.f/((c->traits&4)?2:1);
        if (source<entry->w && !quad(x,py,width,ph,(entry->x+source+0.5f)/atlas_size,(entry->y+(part?14:0)+0.5f)/atlas_size,
            SDL_min(sourceWidth,entry->w-source)/atlas_size,(part?2.f:14.f)/atlas_size,fg)) return false;
    }
    return draw_decorations(c,count,y,height,hover,cursor);
}
static bool reserve_cells(size_t capacity) {
    if (capacity<=cell_capacity) return true;
    size_t size=SDL_max(capacity,cell_capacity?cell_capacity*2:1024);
    struct HideGlyphCell *next=realloc(cell_grid,size*sizeof(*next));
    if (!next) return SDL_SetError("Cannot allocate cell grid");
    cell_grid=next; cell_capacity=size; return true;
}
static bool prepare_grid(void) {
    size_t base=(size_t)cols*rows;
    if (!reserve_cells(base)) return false;
    bool retried=false;
restart:
    atlas_full=false;
    cell_count=base; memset(cell_grid,0,base*sizeof(*cell_grid));
    for (size_t i=0;i<command_count;++i) {
        const GlyphCommand *c=&commands[i];
        AtlasEntry *entry=atlas_entry(c,-1,-1);
        if (!entry) {
            if (!atlas_full) return false;
            if (retried) return SDL_SetError("Visible glyphs exceed the bounded atlas");
            /* Retire allocations between complete preparations, then replay
             * once so no cell keeps an overwritten atlas rectangle. */
            if (!SDL_FlushRenderer(renderer)) return false;
            clear_atlas_entries(); atlas_x=1; atlas_y=atlas_row=0; retried=true;
            goto restart;
        }
        int full=c->script?1:SDL_max(c->cells,(c->width+7)/8);
        int lo=SDL_max(0,c->x),hi=SDL_min(cols,c->x+full);
        if (c->clip_width>=0) { lo=SDL_max(lo,c->clip_x); hi=SDL_min(hi,c->clip_x+c->clip_width); }
        for (int x=lo;x<hi;++x) {
            size_t index=(size_t)c->y*cols+x;
            struct HideGlyphCell prior=cell_grid[index];
            uint32_t previous=0,depth=1;
            if (!c->cells && prior.geometry.z) {
                depth=prior.paint.w+1;
                if (depth>16) return SDL_SetError("Too many zero-advance glyph overlays");
                if (!reserve_cells(cell_count+1)) return false;
                cell_grid[cell_count]=prior; previous=(uint32_t)++cell_count;
            }
            cell_grid[index]=(struct HideGlyphCell){
                {(uint32_t)entry->x|((uint32_t)entry->y<<16),(uint32_t)entry->w|((uint32_t)entry->h<<16),
                 (uint32_t)full|((uint32_t)(x-c->x)<<16)|((uint32_t)(c->script?c->natural:0)<<HIDE_CELL_NATURAL_SHIFT)|((uint32_t)c->script<<HIDE_CELL_SCRIPT_SHIFT),previous},
                {c->fg,c->bg,(c->cells?1u:0u)|(c->text?0u:2u)|(c->traits&24u),depth}};
        }
    }
    size_t bytes=cell_count*sizeof(*cell_grid);
    if (bytes>UINT32_MAX) return SDL_SetError("Cell grid is too large");
    if (bytes>cell_buffer_capacity) {
        SDL_DestroyGPURenderState(glyph_state); glyph_state=NULL;
        SDL_ReleaseGPUBuffer(gpu,cell_buffer); SDL_ReleaseGPUTransferBuffer(gpu,cell_transfer);
        SDL_GPUBufferCreateInfo buffer={SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ,(Uint32)bytes,0};
        SDL_GPUTransferBufferCreateInfo transfer={SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,(Uint32)bytes,0};
        cell_buffer=SDL_CreateGPUBuffer(gpu,&buffer); cell_transfer=SDL_CreateGPUTransferBuffer(gpu,&transfer);
        if (!cell_buffer || !cell_transfer) return false;
        cell_buffer_capacity=bytes;
        SDL_GPURenderStateCreateInfo state={0}; state.fragment_shader=glyph_shader; state.num_storage_buffers=1; state.storage_buffers=&cell_buffer;
        glyph_state=SDL_CreateGPURenderState(renderer,&state); if (!glyph_state) return false;
    }
    void *mapped=SDL_MapGPUTransferBuffer(gpu,cell_transfer,true); if (!mapped) return false;
    memcpy(mapped,cell_grid,bytes); SDL_UnmapGPUTransferBuffer(gpu,cell_transfer);
    SDL_GPUCommandBuffer *upload=SDL_AcquireGPUCommandBuffer(gpu); if (!upload) return false;
    SDL_GPUCopyPass *copy=SDL_BeginGPUCopyPass(upload);
    SDL_GPUTransferBufferLocation source={cell_transfer,0}; SDL_GPUBufferRegion destination={cell_buffer,0,(Uint32)bytes};
    SDL_UploadToGPUBuffer(copy,&source,&destination,true); SDL_EndGPUCopyPass(copy);
    if (!SDL_SubmitGPUCommandBuffer(upload)) return false;
    ++grid_uploads; grid_bytes+=bytes; grid_ready=true; return true;
}
int thc_present(void) {
    if (draw_failed) return 0;
    SDL_SetRenderDrawColor(renderer,0,0,0,255);
    if (!SDL_RenderClear(renderer)) return 0;
    SDL_Rect clip={origin_x,origin_y,cell_x(cols),cell_y(rows)};
    if (!SDL_SetRenderClipRect(renderer,&clip)) return 0;
    cursor_drawn=cursor_phase();
    SDL_FRect target={(float)origin_x,(float)origin_y,(float)cell_x(cols),(float)cell_y(rows)};
    if (gpu) {
        if (!grid_ready && !prepare_grid()) return 0;
        float uniforms[12]={(float)cols,(float)rows,atlas_size,0,
            cursor_present && cursor_drawn?(float)cursor_x:-1,cursor_present && cursor_drawn?(float)cursor_y:-1,
            left_down?-1:(float)mouse_x,left_down?-1:(float)mouse_y,
            (float)cell_x(cols),(float)cell_y(rows),crt_filter?1.f:0.f,(float)(cell_height*scale/16)};
        if (!SDL_SetGPURenderStateFragmentUniforms(glyph_state,0,uniforms,sizeof(uniforms)) || !SDL_SetGPURenderState(renderer,glyph_state) ||
            !SDL_RenderTexture(renderer,texture,NULL,&target) || !SDL_SetGPURenderState(renderer,NULL)) return 0;
        ++draw_batches;
    } else {
        for (size_t i=0;i<command_count;++i) if (!draw_command(&commands[i])) return 0;

    }
    if (!SDL_SetRenderClipRect(renderer,NULL)) return 0;
    if (!gpu && crt_filter && !draw_crt(&target)) return 0;
    const char *capture=SDL_getenv("THC_EDIT_CAPTURE");
    if (capture && *capture && !thc_capture(capture)) return 0;
    return SDL_RenderPresent(renderer);
}
uint64_t thc_event_age_ns(void) { return event_age; }
/* Diagnostic counters for native regression/profiling, never an editor input. */
void thc_atlas_stats(uint64_t *uploads,uint64_t *bytes,uint64_t *batches) { *uploads=atlas_uploads; *bytes=atlas_bytes; *batches=draw_batches; }
void thc_grid_stats(uint64_t *uploads,uint64_t *bytes) { *uploads=grid_uploads; *bytes=grid_bytes; }
int thc_capture(const char *path) {
    SDL_Surface *surface = SDL_RenderReadPixels(renderer, NULL);
    if (!surface) return 0;
    bool ok = SDL_SaveBMP(surface, path);
    SDL_DestroySurface(surface);
    return ok;
}
static int modifiers(SDL_Keymod m) {
    return ((m & SDL_KMOD_SHIFT) ? 1 : 0) | ((m & SDL_KMOD_CTRL) ? 2 : 0) |
           ((m & SDL_KMOD_ALT) ? 4 : 0) | ((m & SDL_KMOD_GUI) ? 8 : 0);
}
static bool modifier_key(SDL_Keycode key) {
    return key >= SDLK_LCTRL && key <= SDLK_RGUI;
}
static int keycode(SDL_Keycode key) {
    if (key >= SDLK_F1 && key <= SDLK_F12) return -101 - (int)(key - SDLK_F1);
    if (key >= SDLK_F13 && key <= SDLK_F24) return -113 - (int)(key - SDLK_F13);
    switch (key) {
    case SDLK_KP_PLUS: return '+'; case SDLK_KP_MINUS: return '-';
    case SDLK_UP: return -1; case SDLK_DOWN: return -2; case SDLK_LEFT: return -3; case SDLK_RIGHT: return -4;
    case SDLK_HOME: return -5; case SDLK_END: return -6; case SDLK_PAGEUP: return -7; case SDLK_PAGEDOWN: return -8;
    case SDLK_TAB: return -9; case SDLK_RETURN: case SDLK_KP_ENTER: return -10; case SDLK_ESCAPE: return -11;
    case SDLK_BACKSPACE: return -12; case SDLK_DELETE: return -13; case SDLK_INSERT: return -14;
    default: return key < 0x110000 ? (int)key : INT_MIN;
    }
}
static void pointer(float x, float y, int32_t *event) {
    int ww, wh;
    SDL_GetWindowSize(window, &ww, &wh);
    /* Floor is essential outside the grid: C integer truncation would hit cell 0. */
    event[1] = (int)floor((x * pixel_w / SDL_max(1, ww) - origin_x) / (8 * scale));
    event[2] = (int)floor((y * pixel_h / SDL_max(1, wh) - origin_y) / (cell_height * scale));
    event[4] = modifiers(SDL_GetModState());
    mouse_x = event[1]; mouse_y = event[2];
    if (mouse_x >= 0 && mouse_x < cols && mouse_y >= 0 && mouse_y < rows) SDL_HideCursor();
    else SDL_ShowCursor();
}
void thc_post_command(int command, int generation) {
    SDL_Event e; SDL_zero(e); e.type = command_event; e.user.code = command; e.user.data1 = (void *)(intptr_t)generation; SDL_PushEvent(&e);
}
void thc_post_window(int ident, int generation) {
    SDL_Event e; SDL_zero(e); e.type = dock_event; e.user.code = ident; e.user.data1 = (void *)(intptr_t)generation; SDL_PushEvent(&e);
}
void thc_raise(void) {
#ifdef __APPLE__
    thc_dock_raise(SDL_GetPointerProperty(SDL_GetWindowProperties(window), SDL_PROP_WINDOW_COCOA_WINDOW_POINTER, NULL));
#else
    SDL_RestoreWindow(window); SDL_RaiseWindow(window);
#endif
}
/* SDL_PushEvent is thread-safe: decoded frames wake the main thread directly. */
void thc_wake(void) {
    if (!SDL_HasEvent(wake_event)) {
        SDL_Event e; SDL_zero(e); e.type = wake_event; SDL_PushEvent(&e);
    }
}
/* Only consume the consecutive run: never move across release/key boundaries. */
static void latest_motion(SDL_Event *event) {
    SDL_Event next;
    while (SDL_PeepEvents(&next, 1, SDL_PEEKEVENT, SDL_EVENT_FIRST, SDL_EVENT_LAST) == 1 &&
           next.type == SDL_EVENT_MOUSE_MOTION && next.motion.windowID == event->motion.windowID &&
           next.motion.which == event->motion.which && next.motion.state == event->motion.state) {
        Uint64 oldest=event->common.timestamp;
        SDL_PeepEvents(event, 1, SDL_GETEVENT, SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_MOTION);
        if (oldest && (!event->common.timestamp || oldest<event->common.timestamp)) event->common.timestamp=oldest;
    }
}
static double wheel_delta(const SDL_Event *event) {
    return event->wheel.y * (event->wheel.direction == SDL_MOUSEWHEEL_FLIPPED ? -1 : 1);
}
static int delivered(const SDL_Event *event,int32_t *out) {
    Uint64 now=SDL_GetTicksNS(),stamp=event->common.timestamp;
    event_age=out[0]!=0 && stamp && now>=stamp?now-stamp:0;
    return 1;
}
static int idle_event(int32_t *out) {
    event_age=0;
    out[0] = cursor_present && cursor_drawn != cursor_phase() ? 8 : 0;
    return 1;
}
int thc_wait(int32_t *out) {
    SDL_Event e;
    event_age=0;
    memset(out, 0, 6 * sizeof(*out));
    Uint64 deadline = SDL_GetTicks() + 100;
    for (;;) {
        Uint64 now = SDL_GetTicks();
        if (now >= deadline) return idle_event(out);
        SDL_ClearError();
        if (!SDL_WaitEventTimeout(&e, (Sint32)(deadline - now))) return *SDL_GetError() ? 0 : idle_event(out);
        if (e.type == wake_event) return delivered(&e,out);
        if (e.type == command_event) { out[0] = 11; out[1] = e.user.code; out[2] = (int32_t)(intptr_t)e.user.data1; return delivered(&e,out); }
        if (e.type == dock_event) { out[0] = 16; out[1] = e.user.code; out[2] = (int32_t)(intptr_t)e.user.data1; return delivered(&e,out); }
        switch (e.type) {
        case SDL_EVENT_QUIT: case SDL_EVENT_WINDOW_CLOSE_REQUESTED: out[0] = 6; return delivered(&e,out);
        case SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED: case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
#ifdef __APPLE__
            thc_file_drag_close();
#endif
            geometry(); refresh_pointer(); out[0] = 5; out[1] = cols; out[2] = rows; return delivered(&e,out);
        case SDL_EVENT_WINDOW_EXPOSED: out[0] = 8; return delivered(&e,out);
        case SDL_EVENT_WINDOW_MOUSE_ENTER:
            refresh_pointer(); out[0] = 12; out[1] = mouse_x; out[2] = mouse_y; return delivered(&e,out);
        case SDL_EVENT_WINDOW_MOUSE_LEAVE:
            clear_pointer();
            out[0] = 12; out[1] = out[2] = -1; return delivered(&e,out);
        case SDL_EVENT_WINDOW_FOCUS_GAINED:
            out[0] = 13; out[1] = modifiers(SDL_GetModState()); return delivered(&e,out);
        case SDL_EVENT_WINDOW_FOCUS_LOST:
            thc_cancel_file_drag();
            clear_pointer(); left_down = false; suppress_option_text = false;
            SDL_CaptureMouse(false); out[0] = 7; return delivered(&e,out);
        case SDL_EVENT_KEY_DOWN: {
            suppress_option_text = false;
            cursor_epoch = SDL_GetTicks();
            int key = keycode(e.key.key), mods = modifiers(e.key.mod);
            if (modifier_key(e.key.key)) { out[0] = 13; out[1] = mods; return delivered(&e,out); }
#ifdef SDL_PLATFORM_MACOS
            /* Command shortcuts use the layout's unmodified scalar; Option
             * may otherwise compose a different printable character. */
            if (mods & 8) {
                SDL_Keycode plain = SDL_GetKeyFromScancode(e.key.scancode, SDL_KMOD_NONE, false);
                if (plain != SDLK_UNKNOWN) key = keycode(plain);
            }
#endif
            /* Printable unmodified keys arrive only through TEXT_INPUT (IME/layout aware). */
            if (key == INT_MIN || (key >= 0 && !(mods & 14))) break;
            if (key >= 0) {
#ifdef SDL_PLATFORM_MACOS
                /* Cocoa consumes registered accelerators before SDL delivery.
                 * Unclaimed Command keys reach the prepared map; Option composes text. */
                if (((mods & 14) == 4 && ((key >= '0' && key <= '9') || key == '+' || key == '=' || key == '-')) ||
                    (mods & 8)) suppress_option_text = true;
                else if ((mods & 4) && !(mods & 10)) break;
#else
                /* AltGr produces TEXT_INPUT, not Alt menu/Control shortcuts.
                 * Left Alt remains available for editor menu shortcuts. */
                if (e.key.mod & (SDL_KMOD_MODE | SDL_KMOD_RALT)) break;
#endif
            }
            out[0] = 1; out[1] = key; out[2] = mods; return delivered(&e,out);
        }
        case SDL_EVENT_KEY_UP:
            suppress_option_text = false;
            if (modifier_key(e.key.key)) { out[0] = 13; out[1] = modifiers(e.key.mod); return delivered(&e,out); }
            break;
        case SDL_EVENT_DROP_FILE:
            SDL_free(input_text); input_text = SDL_strdup(e.drop.data);
            if (!input_text) return 0;
            out[0] = 14; return delivered(&e,out);
        case SDL_EVENT_TEXT_INPUT:
            if (suppress_option_text) { suppress_option_text = false; break; }
            cursor_epoch = SDL_GetTicks();
            SDL_free(input_text); input_text = SDL_strdup(e.text.text);
            if (!input_text) return 0;
            out[0] = 2; return delivered(&e,out);
        case SDL_EVENT_MOUSE_BUTTON_DOWN:
            if (e.button.button != SDL_BUTTON_LEFT && e.button.button != SDL_BUTTON_RIGHT) break;
            if (e.button.button == SDL_BUTTON_LEFT) { left_down = true; SDL_CaptureMouse(true); }
            out[0] = 3; out[3] = e.button.clicks; out[5] = e.button.button;
            pointer(e.button.x, e.button.y, out); return delivered(&e,out);
        case SDL_EVENT_MOUSE_BUTTON_UP:
            if (e.button.button != SDL_BUTTON_LEFT) break;
            left_down = false; SDL_CaptureMouse(false);
            out[0] = 4; pointer(e.button.x, e.button.y, out); return delivered(&e,out);
        case SDL_EVENT_MOUSE_MOTION: {
            latest_motion(&e);
            int old_x = mouse_x, old_y = mouse_y;
            bool was_visible = SDL_CursorVisible();
            pointer(e.motion.x, e.motion.y, out);
            if (mouse_x == old_x && mouse_y == old_y && SDL_CursorVisible() == was_visible) break;
            out[0] = left_down ? 3 : 12; return delivered(&e,out);
        }
        case SDL_EVENT_MOUSE_WHEEL: {
            double delta = wheel_delta(&e);
            SDL_Event next;
            while (SDL_PeepEvents(&next, 1, SDL_PEEKEVENT, SDL_EVENT_FIRST, SDL_EVENT_LAST) == 1 &&
                   next.type == SDL_EVENT_MOUSE_WHEEL && next.wheel.windowID == e.wheel.windowID &&
                   next.wheel.which == e.wheel.which && next.wheel.mouse_x == e.wheel.mouse_x &&
                   next.wheel.mouse_y == e.wheel.mouse_y && wheel_delta(&next) * delta > 0) {
                SDL_PeepEvents(&next, 1, SDL_GETEVENT, SDL_EVENT_MOUSE_WHEEL, SDL_EVENT_MOUSE_WHEEL);
                delta += wheel_delta(&next);
                if (next.common.timestamp && (!e.common.timestamp || next.common.timestamp<e.common.timestamp)) e.common.timestamp=next.common.timestamp;
            }
            if (!isfinite(delta) || delta == 0) break;
            /* Retain fractional travel; a tiny momentum sample is not a wheel notch. */
            if (delta * wheel_remainder < 0) wheel_remainder = 0;
            wheel_remainder += delta;
            int steps = (int)SDL_clamp(trunc(wheel_remainder), -256, 256);
            wheel_remainder -= trunc(wheel_remainder);
            if (!steps) break;
            out[0] = 9; pointer(e.wheel.mouse_x, e.wheel.mouse_y, out);
            out[3] = steps;
            return delivered(&e,out);
        }
        default: break;
        }
    }
}

int thc_system_dark(void) { return SDL_GetSystemTheme() != SDL_SYSTEM_THEME_LIGHT; }
