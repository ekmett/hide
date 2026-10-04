/* Run tools/check-native.sh from the repository root.
 * Uses SDL's event queue and dummy video driver; no real window or user input.
 */
#include "../cbits/window.h"
#include "../cbits/unicode.h"
#include <SDL3/SDL.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

static void check(SDL_Keycode key, SDL_Keymod mods, const char *text, int expected) {
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event e;
    SDL_zero(e); e.type = SDL_EVENT_KEY_DOWN; e.key.key = key; e.key.mod = mods;
    assert(SDL_PushEvent(&e));
    if (text) {
        SDL_zero(e); e.type = SDL_EVENT_TEXT_INPUT; e.text.text = text;
        assert(SDL_PushEvent(&e));
    }
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    int32_t out[6]; assert(thc_wait(out));
    if (out[0] != expected) {
        fprintf(stderr, "key=%u mods=%u: expected event %d, got %d\n", key, mods, expected, out[0]);
        assert(out[0] == expected);
    }
    if (expected == 1 && key >= SDLK_0 && key <= SDLK_9 && (mods & SDL_KMOD_ALT))
        assert(out[1] == (int32_t)key && (out[2] & 4));
    if (expected == 2) assert(strcmp(thc_text(), text) == 0);
}

#ifdef SDL_PLATFORM_MACOS
static void check_command_completion(SDL_Scancode scan, SDL_Keycode composed, const char *text, int shortcut) {
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event e;
    SDL_zero(e); e.type = SDL_EVENT_KEY_DOWN; e.key.key = composed;
    e.key.scancode = scan; e.key.mod = SDL_KMOD_GUI;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_TEXT_INPUT; e.text.text = text;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    int32_t out[6]; assert(thc_wait(out));
    assert(out[0] == 1 && out[1] == shortcut && out[2] == 8);
    assert(thc_wait(out) && out[0] == 6); /* Paired composed text is consumed. */
}
#endif

static void check_mouse_cell(SDL_Renderer *renderer, int row, int cell_height, bool hovered) {
    uint16_t glyph[16], accent[16], bright[16];
    for (int i = 0; i < 16; ++i) { glyph[i] = 0xc000; accent[i] = 0x3000; bright[i] = 0x0c00; }
    assert(thc_begin());
    for (int x = 0; x < 2; ++x) {
        thc_glyph(x, row, 1, 8, glyph, 0xaaaaaa, 0x0000aa, 0);
        thc_glyph(x, row, 0, 8, accent, 0x00aaaa, 0, 0);
        thc_glyph(x, row, 0, 8, bright, 0xffffff, 0, 0);
    }
    for (int repeat = 0; repeat < 2; ++repeat) {
        assert(thc_present());
        SDL_Surface *frame = SDL_RenderReadPixels(renderer, NULL);
        assert(frame);
        for (int cell = 0; cell < 2; ++cell) for (int sample = 0; sample < 4; ++sample) {
            Uint8 r, g, b, a;
            assert(SDL_ReadSurfacePixel(frame, cell * 16 + sample * 4, row * cell_height * 2, &r, &g, &b, &a));
            Uint32 normal[] = {0xaaaaaa, 0x00aaaa, 0xffffff, 0x0000aa};
            Uint32 inverted[] = {0x000000, 0xaa0000, 0x555555, 0xaa5500};
            Uint32 expected = hovered && cell == 1 ? inverted[sample] : normal[sample];
            assert((((Uint32)r << 16) | ((Uint32)g << 8) | b) == expected);
        }
        SDL_DestroySurface(frame);
    }
}

static void wait_cursor_blink(void) {
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    Uint64 started = SDL_GetTicks();
    int32_t out[6];
    do {
        assert(thc_wait(out));
        assert(out[0] == 0 || out[0] == 8);
    } while (out[0] == 0 && SDL_GetTicks() - started < 2000);
    assert(out[0] == 8); /* An idle phase transition requests a repaint. */
}

static void check_crt(SDL_Renderer *renderer, int lines, double pitch) {
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event leave; SDL_zero(leave); leave.type = SDL_EVENT_WINDOW_MOUSE_LEAVE;
    assert(SDL_PushEvent(&leave));
    int32_t event[6]; assert(thc_wait(event) && event[0] == 12);
    uint16_t solid[16];
    for (int i = 0; i < 16; ++i) solid[i] = 0xffff;
    assert(thc_begin());
    for (int y = 0; y < lines; ++y) for (int x = 0; x < 80; ++x)
        thc_glyph(x, y, 1, 8, solid, 0xffffff, 0, 0);
    for (int enabled = 0; enabled < 3; ++enabled) {
        thc_crt_filter(enabled == 1);
        for (int repeat = 0; repeat < 2; ++repeat) {
            assert(thc_present());
            SDL_Surface *frame = SDL_RenderReadPixels(renderer, NULL);
            assert(frame);
            Uint8 center, edge, scanline, g, b, a;
            assert(SDL_ReadSurfacePixel(frame, frame->w/2, frame->h/2, &center, &g, &b, &a));
            assert(SDL_ReadSurfacePixel(frame, 0, 0, &edge, &g, &b, &a));
            assert(SDL_ReadSurfacePixel(frame, frame->w/2, frame->h/2 + (pitch >= 2 ? (int)pitch-1 : 1), &scanline, &g, &b, &a));
            if (enabled == 1) {
                assert(center >= 250 && edge < center - 50);
                if (pitch < 2) assert(scanline >= center - 1); /* One pixel per glyph row: vignette only. */
                else assert(scanline >= center - 28 && scanline < center - 16);
            }
            else assert(center == 255 && edge == 255 && scanline == 255);
            SDL_DestroySurface(frame);
        }
    }
}

static void check_diffusion(void) {
    uint32_t source[64*64], output[16*16], repeated[16*16];
    for (int i=0;i<64*64;++i) source[i]=0x80800000; /* Half-covered red. */
    assert(thc_unicode_downsample(source,16,16,output));
    assert(thc_unicode_downsample(source,16,16,repeated));
    assert(!memcmp(output,repeated,sizeof(output)));
    int coverage=0, lower=0, upper=0;
    for (int i=0;i<16*16;++i) {
        unsigned alpha=output[i]>>24;
        assert(alpha%85==0 && ((output[i]>>16)&255)==alpha && !(output[i]&0xffff));
        coverage+=alpha; lower+=alpha==85; upper+=alpha==170;
    }
    assert(coverage>=127*256 && coverage<=129*256 && lower && upper);
    /* Preserve average coverage instead of rounding every cell the same way. */
    memset(source,0,sizeof(source));
    assert(thc_unicode_downsample(source,16,16,output));
    for (int i=0;i<16*16;++i) assert(!output[i]);
    for (int i=0;i<64*64;++i) source[i]=0xff5599cc;
    assert(thc_unicode_downsample(source,16,16,output));
    for (int i=0;i<16*16;++i) assert(output[i]==0xff5599cc);
}

static void check_unicode(SDL_Renderer *renderer) {
    uint64_t hashes[3]={0};
    for (int mode=0;mode<3;++mode) {
        assert(thc_begin()); thc_pixelate_unicode(mode==1);
        assert(thc_unicode(1,1,2,"👩🏽‍💻",0xffffff,0x0000aa,0));
        assert(thc_unicode(3,1,1,"é",0xffff55,0x0000aa,0));
        assert(thc_present());
        SDL_Surface *image=SDL_RenderReadPixels(renderer,NULL);
        assert(image);
        unsigned drawn=0;
        for (int y=0;y<image->h;++y) for (int x=0;x<image->w;++x) {
            Uint8 r,g,b,a; assert(SDL_ReadSurfacePixel(image,x,y,&r,&g,&b,&a));
            if (r || g) ++drawn;
            hashes[mode]=hashes[mode]*33+r*65536u+g*256u+b;
        }
        assert(drawn>10); SDL_DestroySurface(image);
    }
    assert(hashes[0]!=hashes[1] && hashes[0]==hashes[2]);
    thc_pixelate_unicode(0);
}

static void check_font_traits(SDL_Renderer *renderer) {
    uint16_t glyph[16];
    for (int y=0;y<16;++y) glyph[y]=0x1000;
    for (int bitmap=0;bitmap<3;++bitmap) {
        uint64_t hashes[5]={0};
        thc_pixelate_unicode(bitmap==2);
        for (int style=0;style<5;++style) {
            unsigned traits=style==4?0:(unsigned)style;
            assert(thc_begin());
            if (!bitmap) thc_glyph(1,1,1,8,glyph,0xffffff,0,traits);
            else assert(thc_unicode(1,1,1,"f",0xffffff,0,traits));
            assert(thc_present());
            SDL_Surface *image=SDL_RenderReadPixels(renderer,NULL);
            assert(image);
            for (int y=0;y<image->h;++y) for (int x=0;x<image->w;++x) {
                Uint8 r,g,b,a; assert(SDL_ReadSurfacePixel(image,x,y,&r,&g,&b,&a));
                hashes[style]=hashes[style]*33+r*65536u+g*256u+b;
            }
            SDL_DestroySurface(image);
        }
        for (int a=0;a<4;++a) for (int b=a+1;b<4;++b) assert(hashes[a]!=hashes[b]);
        assert(hashes[0]==hashes[4]); /* Cache returns the original regular glyph. */
    }
    thc_pixelate_unicode(0);
    assert(thc_begin());
    thc_glyph(1,1,2,8,glyph,0xffffff,0,4); /* Narrow bitmap stretched into two cells. */
    assert(thc_unicode(4,1,2,"f",0xffffff,0,4));
    assert(thc_present());
    SDL_Surface *wide=SDL_RenderReadPixels(renderer,NULL);
    assert(wide);
    int columns,rows; thc_size(&columns,&rows);
    int top=wide->h/rows;
    unsigned left=0,right=0;
    for (int y=top;y<2*top;++y) for (int x=0;x<32;++x) {
        Uint8 r,g,b,a; assert(SDL_ReadSurfacePixel(wide,16+x,y,&r,&g,&b,&a));
        if (r) { if(x<16)++left;else ++right; }
    }
    assert(left>0 && right==0); /* This thin bitmap's stroke doubles horizontally. */
    Uint8 r,g,b,a;
    assert(SDL_ReadSurfacePixel(wide,16+12,top,&r,&g,&b,&a) && r==255);
    assert(SDL_ReadSurfacePixel(wide,16+6,top,&r,&g,&b,&a) && r==0);
    SDL_DestroySurface(wide);
}

static void check_geometry(int lines, int cell_height) {
    assert(SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "dummy"));
    if (!thc_open("software", 2, 80, lines, cell_height)) {
        fprintf(stderr, "Open dummy window: %s\n", thc_error());
        assert(0);
    }
    int count, width, height, cols, rows;
    SDL_Window **windows = SDL_GetWindows(&count);
    assert(windows && count == 1);
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height));
    assert(width == 1280 && height == 800);
    thc_size(&cols, &rows);
    assert(cols == 80 && rows == lines);
    check_crt(SDL_GetRenderer(windows[0]), lines, cell_height/8.0);
    check_unicode(SDL_GetRenderer(windows[0]));
    check_font_traits(SDL_GetRenderer(windows[0]));
    assert(thc_begin());
    uint16_t glyph[16];
    for (int i = 0; i < 16; ++i) glyph[i] = 0xffff;
    thc_glyph(0, lines - 1, 1, 8, glyph, 0xff0000, 0, 0);
    thc_cursor(0, lines - 1);
    assert(thc_present());
    SDL_Surface *frame = SDL_RenderReadPixels(SDL_GetRenderer(windows[0]), NULL);
    assert(frame && frame->w == 1280 && frame->h == 800);
    Uint8 r, g, b, a;
    assert(SDL_ReadSurfacePixel(frame, 0, 799, &r, &g, &b, &a));
    assert(r == 0 && g == 255 && b == 255); /* Cursor on the last line. */
    SDL_DestroySurface(frame);
    SDL_Renderer *test_renderer = SDL_GetRenderer(windows[0]);
    SDL_free(windows);
    wait_cursor_blink();
    assert(thc_begin());
    thc_glyph(0, lines - 1, 1, 8, glyph, 0xff0000, 0, 0);
    thc_cursor(0, lines - 1);
    assert(thc_present());
    frame = SDL_RenderReadPixels(test_renderer, NULL);
    assert(frame && SDL_ReadSurfacePixel(frame, 0, 799, &r, &g, &b, &a));
    assert(r == 255 && g == 0 && b == 0); /* Default blinking caret is now off. */
    SDL_DestroySurface(frame);
    thc_cursor_blink(0);
    wait_cursor_blink();
    assert(thc_begin());
    thc_glyph(0, lines - 1, 1, 8, glyph, 0xff0000, 0, 0);
    thc_cursor(0, lines - 1);
    assert(thc_present());
    frame = SDL_RenderReadPixels(test_renderer, NULL);
    assert(frame && SDL_ReadSurfacePixel(frame, 0, 799, &r, &g, &b, &a));
    assert(r == 0 && g == 255 && b == 255); /* Disabled blinking stays visible. */
    SDL_DestroySurface(frame);
    for (int i = 0; i < 6; ++i) {
        int32_t idle[6]; assert(thc_wait(idle) && idle[0] == 0);
    }
    thc_cursor_blink(1);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event e;
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_WHEEL;
    e.wheel.y = 1; e.wheel.mouse_x = 7; e.wheel.mouse_y = 799;
    assert(SDL_PushEvent(&e));
    int32_t out[6]; assert(thc_wait(out));
    assert(out[0] == 9 && out[1] == 0 && out[2] == lines - 1);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION;
    e.motion.x = 23; e.motion.y = 799;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    assert(thc_wait(out));
    assert(out[0] == 12 && out[1] == 1 && out[2] == lines - 1);
    check_mouse_cell(test_renderer, lines - 1, cell_height, true);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION; e.motion.x = 25; e.motion.y = 798;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 6); /* Same-cell motion yields no redraw event. */
    assert(SDL_ShowCursor());
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION; e.motion.x = 25; e.motion.y = 798;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 12); /* Visibility changes still refresh the cell cursor. */
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION; e.motion.x = -0.25f; e.motion.y = 799;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 12 && out[1] == -1);
    check_mouse_cell(test_renderer, lines - 1, cell_height, false);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_DOWN;
    e.button.button = SDL_BUTTON_LEFT; e.button.x = 23; e.button.y = 799;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3);
    check_mouse_cell(test_renderer, lines - 1, cell_height, false); /* No cell pointer during a drag. */
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION;
    e.motion.x = 39; e.motion.y = 799;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3 && out[1] == 2);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION; e.motion.x = 40; e.motion.y = 798;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_UP; e.button.button = SDL_BUTTON_LEFT;
    e.button.x = 23; e.button.y = 799;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 4); /* Same-cell drag samples do no work. */
    check_mouse_cell(test_renderer, lines - 1, cell_height, true); /* Release restores the pointer. */
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_DOWN; e.button.button = SDL_BUTTON_LEFT;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3);
    for (int i=0;i<1000;++i) {
        SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION;
        e.motion.x = 16 + i % 500; e.motion.y = 100;
        assert(SDL_PushEvent(&e));
    }
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_UP; e.button.button = SDL_BUTTON_LEFT;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION; e.motion.x = 100; e.motion.y = 100;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3 && out[1] == 32);
    assert(thc_wait(out) && out[0] == 4); /* Never coalesce across release. */
    assert(thc_wait(out) && out[0] == 12 && out[1] == 6);
    for (int i=0;i<1000;++i) {
        SDL_zero(e); e.type = SDL_EVENT_MOUSE_WHEEL; e.wheel.y = 0.125f;
        e.wheel.mouse_x = 20; e.wheel.mouse_y = 100; assert(SDL_PushEvent(&e));
    }
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 9 && out[3] == 125);
    assert(thc_wait(out) && out[0] == 6);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_WHEEL; e.wheel.y = 0.5f;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 6);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_WHEEL; e.wheel.y = 0.5f;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 9 && out[3] == 1);
    thc_wake(); thc_wake();
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 0); /* Frame notification wakes without forced repaint. */
    assert(thc_wait(out) && out[0] == 6); /* Redundant wake signals collapse too. */

    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    thc_post_command(37, 19);
    assert(thc_wait(out) && out[0] == 11 && out[1] == 37 && out[2] == 19);
    thc_post_command(37, 20);
    assert(thc_wait(out) && out[0] == 11 && out[1] == 37 && out[2] == 20);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_DOWN;
    e.button.button = SDL_BUTTON_LEFT; e.button.clicks = 2;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3 && out[3] == 2);
    SDL_zero(e); e.type = SDL_EVENT_WINDOW_FOCUS_LOST;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 7);
    check_mouse_cell(test_renderer, lines - 1, cell_height, false);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_DOWN;
    e.button.button = SDL_BUTTON_RIGHT; e.button.x = 23; e.button.y = 799;
    assert(SDL_PushEvent(&e));
    SDL_zero(e); e.type = SDL_EVENT_QUIT; assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3 && out[5] == SDL_BUTTON_RIGHT && out[1] == 1);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_zero(e); e.type = SDL_EVENT_WINDOW_MOUSE_LEAVE;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 12 && out[1] == -1 && out[2] == -1);
    check_mouse_cell(test_renderer, lines - 1, cell_height, false);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_SetError("A stale SDL error must not turn an idle wake into failure");
    Uint64 start = SDL_GetTicks();
    assert(thc_wait(out) && out[0] == 0);
    assert(SDL_GetTicks() - start >= 50 && SDL_GetTicks() - start < 1000);
    assert(!thc_mode(12, 80, 25));
    assert(!thc_mode(cell_height == 16 ? 8 : 16, 0, 25));
    thc_size(&cols, &rows);
    assert(cols == 80 && rows == lines);
    /* Rejection and failed resize preserve the grid. */
    assert(thc_mode(cell_height == 16 ? 8 : 16, 80, lines == 25 ? 50 : 25));
    thc_size(&cols, &rows);
    assert(cols == 80 && rows == (lines == 25 ? 50 : 25));
    assert(thc_begin() && thc_present()); /* Reallocate the source framebuffer. */
    assert(thc_mode(8, 100, 32));
    thc_size(&cols, &rows);
    assert(cols == 100 && rows == 32);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    assert(thc_scale(1));
    Uint64 resized_at = SDL_GetTicks();
    do { assert(thc_wait(out)); } while (out[0] != 5 && SDL_GetTicks() - resized_at < 2000);
    assert(out[0] == 5 && out[1] == 100 && out[2] == 32); /* Same grid still requests redraw. */
    thc_size(&cols, &rows);
    assert(cols == 100 && rows == 32);
    windows = SDL_GetWindows(&count);
    assert(windows && count == 1 && SDL_GetWindowSizeInPixels(windows[0], &width, &height));
    assert(width == 1700 && height == 544); /* Larger physical tiles, same character grid. */
    assert(thc_scale(0) && thc_scale(-1));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height));
    assert(width == 1500 && height == 480);
    thc_size(&cols, &rows);
    assert(cols == 100 && rows == 32);
    assert(thc_mode(8, 40, 12));
    for (int i = 0; i < 64; ++i) assert(thc_scale(1));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 2560 && height == 768);
    for (int i = 0; i < 64; ++i) assert(thc_scale(-1));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 320 && height == 96);
    assert(thc_scale(0));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 640 && height == 192);
    thc_size(&cols, &rows);
    assert(cols == 40 && rows == 12);
    SDL_free(windows);
    thc_close();
}

static void check_fractional_zoom(void) {
    assert(SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "dummy"));
    assert(thc_open("software", 1.125, 81, 25, 16));
    int count, width, height, cols, rows;
    SDL_Window **windows = SDL_GetWindows(&count);
    assert(windows && count == 1);
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 729 && height == 450);
    thc_size(&cols, &rows); assert(cols == 81 && rows == 25);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event e; SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_DOWN;
    e.button.button = SDL_BUTTON_LEFT; e.button.x = 18.5f; e.button.y = 36.5f;
    assert(SDL_PushEvent(&e));
    int32_t event[6]; assert(thc_wait(event) && event[0] == 3 && event[1] == 2 && event[2] == 2);
    assert(thc_scale(1));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 810 && height == 500);
    thc_size(&cols, &rows); assert(cols == 81 && rows == 25);
    assert(thc_scale(0));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 1296 && height == 800);
    SDL_free(windows);
    thc_close();
    assert(SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "dummy"));
    assert(thc_open("software", 4, 80, 50, 8));
    windows = SDL_GetWindows(&count);
    assert(windows && count == 1);
    check_crt(SDL_GetRenderer(windows[0]), 50, 2); /* Compressed mode at Retina default size. */
    SDL_free(windows);
    thc_close();
}

int main(void) {
    assert(SDL_Init(SDL_INIT_EVENTS));
    check(SDLK_LSHIFT, SDL_KMOD_LSHIFT, NULL, 13);
    check(SDLK_RCTRL, SDL_KMOD_RCTRL, NULL, 13);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event modifier; SDL_zero(modifier);
    modifier.type = SDL_EVENT_KEY_UP; modifier.key.key = SDLK_RCTRL;
    modifier.key.mod = SDL_KMOD_LSHIFT;
    assert(SDL_PushEvent(&modifier));
    int32_t modifier_event[6];
    assert(thc_wait(modifier_event) && modifier_event[0] == 13 && modifier_event[1] == 1);
    modifier.key.key = SDLK_LSHIFT; modifier.key.mod = SDL_KMOD_NONE;
    assert(SDL_PushEvent(&modifier));
    assert(thc_wait(modifier_event) && modifier_event[0] == 13 && modifier_event[1] == 0);
    check(SDLK_A, SDL_KMOD_NONE, "a", 2);
    check(SDLK_A, SDL_KMOD_SHIFT, "A", 2);
    check(SDLK_K, SDL_KMOD_CTRL, NULL, 1); /* WordStar prefix */
    check(SDLK_F3, SDL_KMOD_ALT, NULL, 1);
    check(SDLK_LEFT, SDL_KMOD_ALT, NULL, 1);
    check(SDLK_KP_PLUS, SDL_KMOD_CTRL, NULL, 1);
    check(SDLK_KP_MINUS, SDL_KMOD_CTRL, NULL, 1);
#ifdef SDL_PLATFORM_MACOS
    check(SDLK_N, SDL_KMOD_GUI, NULL, 1); /* Unclaimed Command reaches the shared map. */
    check(SDLK_Z, SDL_KMOD_GUI | SDL_KMOD_SHIFT, NULL, 1);
    const char *option_digits[] = {"¡", "™", "£", "¢", "∞", "§", "¶", "•", "ª"};
    for (int i = 0; i < 9; ++i) {
        check(SDLK_1 + i, SDL_KMOD_ALT, option_digits[i], 1);
        int32_t out[6];
        assert(thc_wait(out) && out[0] == 6); /* Shortcut must consume its composed text. */
    }
    check(SDLK_1, SDL_KMOD_ALT, NULL, 1);
    check(SDLK_A, SDL_KMOD_NONE, "a", 2); /* A later key must not lose its text. */
    check(SDLK_1, SDL_KMOD_ALT, NULL, 1);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event released;
    SDL_zero(released); released.type = SDL_EVENT_KEY_UP; released.key.key = SDLK_1;
    assert(SDL_PushEvent(&released));
    SDL_zero(released); released.type = SDL_EVENT_TEXT_INPUT; released.text.text = "é";
    assert(SDL_PushEvent(&released));
    int32_t out[6]; assert(thc_wait(out) && out[0] == 2 && strcmp(thc_text(), "é") == 0);
    check(SDLK_1, SDL_KMOD_GUI | SDL_KMOD_ALT, NULL, 1);
    SDL_Keycode scale_keys[] = {SDLK_0, SDLK_EQUALS, SDLK_PLUS, SDLK_MINUS};
    const char *scale_text[] = {"º", "≠", "±", "–"};
    for (int i = 0; i < 4; ++i) {
        check(scale_keys[i], SDL_KMOD_ALT, scale_text[i], 1);
        assert(thc_wait(out) && out[0] == 6);
    }
    check_command_completion(SDL_SCANCODE_BACKSLASH, 0x00ab, "«", '\\');
    check_command_completion(SDL_SCANCODE_LEFTBRACKET, 0x201c, "“", '[');
    check_command_completion(SDL_SCANCODE_RIGHTBRACKET, 0x2018, "‘", ']');
    check(SDLK_BACKSLASH, SDL_KMOD_ALT, "«", 2);
    check(SDLK_LEFTBRACKET, SDL_KMOD_ALT, "“", 2);
    check(SDLK_RIGHTBRACKET, SDL_KMOD_ALT, "‘", 2);
    check(SDLK_BACKSLASH, SDL_KMOD_ALT | SDL_KMOD_SHIFT, "»", 2);
    check(SDLK_X, SDL_KMOD_ALT, "≈", 2);
    check(SDLK_F, SDL_KMOD_ALT, "ƒ", 2);
    check(SDLK_E, SDL_KMOD_ALT, NULL, 6); /* Dead key awaits composed text. */
#else
    check(SDLK_F, SDL_KMOD_LALT, NULL, 1); /* Alt menu shortcuts remain. */
    check(SDLK_Q, SDL_KMOD_MODE, "@", 2);
    check(SDLK_Q, SDL_KMOD_RALT | SDL_KMOD_CTRL, "@", 2);
#endif
    SDL_Event dropped; SDL_zero(dropped); dropped.type=SDL_EVENT_DROP_FILE;
    dropped.drop.data="/tmp/a file.hs"; assert(SDL_PushEvent(&dropped));
    int32_t drop_event[6]; assert(thc_wait(drop_event) && drop_event[0]==14);
    assert(strcmp(thc_text(),"/tmp/a file.hs")==0);
    SDL_Quit();
    check_diffusion();
    assert(SDL_SetHint(SDL_HINT_VIDEO_DRIVER,"dummy"));
    assert(thc_open("software",1,80,25,16));
    int count; SDL_Window **single=SDL_GetWindows(&count); assert(single && count==1);
    check_unicode(SDL_GetRenderer(single[0])); /* Cache keys differ even at equal tile sizes. */
    SDL_free(single); thc_close();
    check_geometry(25, 16);
    check_geometry(50, 8);
    check_fractional_zoom();
    puts("native input checks passed");
    return 0;
}

#ifdef __APPLE__
/* This isolated keyboard decoder test has no Cocoa application lifecycle. */
void thc_dock_close(void) {}
void thc_dock_raise(void *window) { (void)window; }
#endif
