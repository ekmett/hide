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
    if (expected == 2) assert(strcmp(thc_text(), text) == 0);
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
    SDL_free(windows);
    SDL_FlushEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST);
    SDL_Event e;
    SDL_zero(e); e.type = SDL_EVENT_MOUSE_WHEEL;
    e.wheel.y = 1; e.wheel.mouse_x = 23; e.wheel.mouse_y = 799;
    assert(SDL_PushEvent(&e));
    int32_t out[6]; assert(thc_wait(out));
    assert(out[0] == 9 && out[1] == 1 && out[2] == lines - 1);
    assert(!thc_mode(12, 80, 25));
    assert(!thc_mode(cell_height == 16 ? 8 : 16, 0, 25));
    thc_size(&cols, &rows);
    assert(cols == 80 && rows == lines); /* Rejection and failed resize preserve the grid. */
    assert(thc_mode(cell_height == 16 ? 8 : 16, 80, lines == 25 ? 50 : 25));
    thc_size(&cols, &rows);
    assert(cols == 80 && rows == (lines == 25 ? 50 : 25));
    assert(thc_begin() && thc_present()); /* Reallocate the source framebuffer. */
    assert(thc_mode(8, 100, 32));
    thc_size(&cols, &rows);
    assert(cols == 100 && rows == 32);
    thc_close();
}

int main(void) {
    assert(SDL_Init(SDL_INIT_EVENTS));
    check(SDLK_A, SDL_KMOD_NONE, "a", 2);
    check(SDLK_A, SDL_KMOD_SHIFT, "A", 2);
    check(SDLK_K, SDL_KMOD_CTRL, NULL, 1); /* WordStar prefix */
    check(SDLK_F3, SDL_KMOD_ALT, NULL, 1);
    check(SDLK_LEFT, SDL_KMOD_ALT, NULL, 1);
#ifdef SDL_PLATFORM_MACOS
    check(SDLK_N, SDL_KMOD_GUI, NULL, 6); /* Native menu alone invokes New. */
    check(SDLK_Z, SDL_KMOD_GUI | SDL_KMOD_SHIFT, NULL, 6);
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
