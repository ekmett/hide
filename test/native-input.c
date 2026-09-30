/* Run from the repository root:
 * cc -Wall -Wextra $(pkg-config --cflags sdl3) cbits/window.c test/native-input.c \
 *   $(pkg-config --libs sdl3) -lm -o /tmp/thc-native-input && /tmp/thc-native-input
 * Uses SDL's event queue and dummy video driver; no real window or user input.
 */
#include "../cbits/window.h"
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

static void check_mouse_cell(SDL_Renderer *renderer, int row, int cell_height, bool hovered) {
    uint16_t glyph[16], accent[16], bright[16];
    for (int i = 0; i < 16; ++i) { glyph[i] = 0xc000; accent[i] = 0x3000; bright[i] = 0x0c00; }
    assert(thc_begin());
    for (int x = 0; x < 2; ++x) {
        thc_glyph(x, row, 1, 8, glyph, 0xaaaaaa, 0x0000aa);
        thc_glyph(x, row, 0, 8, accent, 0x00aaaa, 0);
        thc_glyph(x, row, 0, 8, bright, 0xffffff, 0);
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

static void check_crt(SDL_Renderer *renderer, int lines) {
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event leave; SDL_zero(leave); leave.type = SDL_EVENT_WINDOW_MOUSE_LEAVE;
    assert(SDL_PushEvent(&leave));
    int32_t event[6]; assert(thc_wait(event) && event[0] == 12);
    uint16_t solid[16];
    for (int i = 0; i < 16; ++i) solid[i] = 0xffff;
    assert(thc_begin());
    for (int y = 0; y < lines; ++y) for (int x = 0; x < 80; ++x)
        thc_glyph(x, y, 1, 8, solid, 0xffffff, 0);
    for (int enabled = 0; enabled < 3; ++enabled) {
        thc_crt_filter(enabled == 1);
        for (int repeat = 0; repeat < 2; ++repeat) {
            assert(thc_present());
            SDL_Surface *frame = SDL_RenderReadPixels(renderer, NULL);
            assert(frame);
            Uint8 center, edge, scanline, g, b, a;
            assert(SDL_ReadSurfacePixel(frame, 640, 400, &center, &g, &b, &a));
            assert(SDL_ReadSurfacePixel(frame, 0, 0, &edge, &g, &b, &a));
            assert(SDL_ReadSurfacePixel(frame, 640, 401, &scanline, &g, &b, &a));
            if (enabled == 1) assert(center >= 250 && edge < center - 50 && scanline < center - 30);
            else assert(center == 255 && edge == 255 && scanline == 255);
            SDL_DestroySurface(frame);
        }
    }
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
    check_crt(SDL_GetRenderer(windows[0]), lines);
    assert(thc_begin());
    uint16_t glyph[16];
    for (int i = 0; i < 16; ++i) glyph[i] = 0xffff;
    thc_glyph(0, lines - 1, 1, 8, glyph, 0xff0000, 0);
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
    thc_glyph(0, lines - 1, 1, 8, glyph, 0xff0000, 0);
    thc_cursor(0, lines - 1);
    assert(thc_present());
    frame = SDL_RenderReadPixels(test_renderer, NULL);
    assert(frame && SDL_ReadSurfacePixel(frame, 0, 799, &r, &g, &b, &a));
    assert(r == 255 && g == 0 && b == 0); /* Default blinking caret is now off. */
    SDL_DestroySurface(frame);
    thc_cursor_blink(0);
    wait_cursor_blink();
    assert(thc_begin());
    thc_glyph(0, lines - 1, 1, 8, glyph, 0xff0000, 0);
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
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION;
    e.motion.x = 39; e.motion.y = 799;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3 && out[1] == 2);
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_MOTION; e.motion.x = 40; e.motion.y = 798;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 3 && out[1] == 2); /* Drag events remain uncoalesced. */
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_BUTTON_UP; e.button.button = SDL_BUTTON_LEFT;
    assert(SDL_PushEvent(&e));
    assert(thc_wait(out) && out[0] == 4);
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
    assert(width == 2400 && height == 768); /* Larger physical tiles, same character grid. */
    assert(thc_scale(0) && thc_scale(-1));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height));
    assert(width == 800 && height == 256);
    thc_size(&cols, &rows);
    assert(cols == 100 && rows == 32);
    assert(thc_mode(8, 40, 12));
    for (int i = 0; i < 10; ++i) assert(thc_scale(1));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 2560 && height == 768);
    for (int i = 0; i < 10; ++i) assert(thc_scale(-1));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 320 && height == 96);
    assert(thc_scale(0));
    assert(SDL_GetWindowSizeInPixels(windows[0], &width, &height) && width == 640 && height == 192);
    thc_size(&cols, &rows);
    assert(cols == 40 && rows == 12);
    SDL_free(windows);
    thc_close();
}

int main(void) {
    assert(SDL_Init(SDL_INIT_EVENTS));
    check(SDLK_A, SDL_KMOD_NONE, "a", 2);
    check(SDLK_A, SDL_KMOD_SHIFT, "A", 2);
    check(SDLK_K, SDL_KMOD_CTRL, NULL, 1); /* WordStar prefix */
    check(SDLK_F3, SDL_KMOD_ALT, NULL, 1);
    check(SDLK_LEFT, SDL_KMOD_ALT, NULL, 1);
    check(SDLK_KP_PLUS, SDL_KMOD_CTRL, NULL, 1);
    check(SDLK_KP_MINUS, SDL_KMOD_CTRL, NULL, 1);
#ifdef SDL_PLATFORM_MACOS
    check(SDLK_N, SDL_KMOD_GUI, NULL, 6); /* Native menu alone invokes New. */
    check(SDLK_Z, SDL_KMOD_GUI | SDL_KMOD_SHIFT, NULL, 6);
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
    check(SDLK_1, SDL_KMOD_GUI | SDL_KMOD_ALT, NULL, 6);
    SDL_Keycode scale_keys[] = {SDLK_0, SDLK_EQUALS, SDLK_PLUS, SDLK_MINUS};
    const char *scale_text[] = {"º", "≠", "±", "–"};
    for (int i = 0; i < 4; ++i) {
        check(scale_keys[i], SDL_KMOD_ALT, scale_text[i], 1);
        assert(thc_wait(out) && out[0] == 6);
    }
    check(SDLK_X, SDL_KMOD_ALT, "≈", 2);
    check(SDLK_F, SDL_KMOD_ALT, "ƒ", 2);
    check(SDLK_E, SDL_KMOD_ALT, NULL, 6); /* Dead key awaits composed text. */
#else
    check(SDLK_F, SDL_KMOD_LALT, NULL, 1); /* Alt menu shortcuts remain. */
    check(SDLK_Q, SDL_KMOD_MODE, "@", 2);
    check(SDLK_Q, SDL_KMOD_RALT | SDL_KMOD_CTRL, "@", 2);
#endif
    SDL_Quit();
    check_geometry(25, 16);
    check_geometry(50, 8);
    puts("native input checks passed");
    return 0;
}
