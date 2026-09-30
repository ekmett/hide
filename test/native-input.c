/* Run from the repository root:
 * cc -Wall -Wextra $(pkg-config --cflags sdl3) cbits/window.c test/native-input.c \
 *   $(pkg-config --libs sdl3) -lm -o /tmp/thc-native-input && /tmp/thc-native-input
 * Uses only SDL's event queue: no window, clipboard or user input is touched.
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
    puts("native input checks passed");
    return 0;
}
