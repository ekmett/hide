#include "window.h"
#include <SDL3/SDL.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

static SDL_Window *window;
static SDL_Renderer *renderer;
static SDL_Texture *texture;
static uint32_t *pixels;
static int cols, rows, scale, origin_x, origin_y, pixel_w, pixel_h;
static char *input_text;
static char *clipboard_text;
static bool left_down;
static Uint32 command_event;

const char *thc_error(void) { return SDL_GetError(); }
const char *thc_backend(void) { return SDL_GetRendererName(renderer); }
const char *thc_text(void) { return input_text ? input_text : ""; }
const char *thc_clipboard(void) {
    SDL_free(clipboard_text);
    clipboard_text = SDL_GetClipboardText();
    return clipboard_text ? clipboard_text : "";
}
void thc_set_clipboard(const char *s) { SDL_SetClipboardText(s); }

void thc_close(void) {
    SDL_StopTextInput(window);
    SDL_DestroyTexture(texture); texture = NULL;
    SDL_DestroyRenderer(renderer); renderer = NULL;
    SDL_DestroyWindow(window); window = NULL;
    free(pixels); pixels = NULL;
    SDL_free(input_text); input_text = NULL;
    SDL_free(clipboard_text); clipboard_text = NULL;
    SDL_Quit();
}

static void geometry(void) {
    SDL_GetRenderOutputSize(renderer, &pixel_w, &pixel_h);
    cols = SDL_clamp(pixel_w / (8 * scale), 1, 512);
    rows = SDL_clamp(pixel_h / (16 * scale), 1, 256);
    origin_x = (pixel_w - cols * 8 * scale) / 2;
    origin_y = (pixel_h - rows * 16 * scale) / 2;
}

int thc_open(const char *backend, int requested_scale, int requested_cols, int requested_rows) {
    if (!SDL_Init(SDL_INIT_VIDEO)) return 0;
    command_event = SDL_RegisterEvents(1);
    window = SDL_CreateWindow("Turbo Haskell", 1280, 800, SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY);
    if (!window) return 0;
    renderer = SDL_CreateRenderer(window, backend);
    if (!renderer) return 0;
    SDL_SetRenderVSync(renderer, 1);
    int pw, ph, ww, wh;
    SDL_GetWindowSizeInPixels(window, &pw, &ph);
    SDL_GetWindowSize(window, &ww, &wh);
    double density = (double)pw / SDL_max(1, ww);
    scale = requested_scale ? requested_scale : SDL_max(1, (int)lround(2 * density));
    SDL_SetWindowSize(window, (int)lround(requested_cols * 8 * scale / density), (int)lround(requested_rows * 16 * scale / density));
    SDL_SetWindowMinimumSize(window, (int)ceil(40 * 8 * scale / density), (int)ceil(12 * 16 * scale / density));
    geometry();
    return SDL_StartTextInput(window);
}

void thc_size(int *w, int *h) { geometry(); *w = cols; *h = rows; }
int thc_begin(void) {
    geometry();
    int w = cols * 8, h = rows * 16;
    float tw = 0, th = 0;
    if (texture) SDL_GetTextureSize(texture, &tw, &th);
    if (!texture || tw != w || th != h) {
        SDL_DestroyTexture(texture); texture = NULL;
        free(pixels); pixels = NULL;
        texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, w, h);
        if (!texture) return 0;
        SDL_SetTextureScaleMode(texture, SDL_SCALEMODE_NEAREST);
        SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_NONE);
        pixels = calloc((size_t)w * h, sizeof(*pixels));
        if (!pixels) { SDL_SetError("Cannot allocate cell framebuffer"); return 0; }
    }
    for (int i = 0; i < w * h; ++i) pixels[i] = 0xff000000;
    return 1;
}

/* Bitmap composition keeps all glyph/background edges on the same cell grid.
 * ponytail: upload one small frame per event; use an atlas if profiling demands it. */
void thc_glyph(int x, int y, int cells, int glyph_width, const uint16_t *bits, uint32_t fg, uint32_t bg) {
    int width = cols * 8;
    if (!pixels || y < 0 || y >= rows) return;
    for (int dy = 0; dy < 16; ++dy) for (int dx = 0; dx < SDL_max(cells * 8, glyph_width); ++dx) {
        int px = x * 8 + dx;
        if (px < 0 || px >= width) continue;
        bool ink = dx < glyph_width && dx < 16 && (bits[dy] & (0x8000u >> dx));
        if (ink || cells) pixels[(y * 16 + dy) * width + px] = 0xff000000 | (ink ? fg : bg);
    }
}
void thc_cursor(int x, int y) {
    if (!pixels || x < 0 || x >= cols || y < 0 || y >= rows) return;
    for (int dy = 14; dy < 16; ++dy) for (int dx = 0; dx < 8; ++dx)
        pixels[(y * 16 + dy) * cols * 8 + x * 8 + dx] ^= 0x00ffffff;
}
int thc_present(void) {
    if (!SDL_UpdateTexture(texture, NULL, pixels, cols * 8 * sizeof(*pixels))) return 0;
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    if (!SDL_RenderClear(renderer)) return 0;
    SDL_FRect target = {(float)origin_x, (float)origin_y, (float)(cols * 8 * scale), (float)(rows * 16 * scale)};
    if (!SDL_RenderTexture(renderer, texture, NULL, &target)) return 0;
    const char *capture = SDL_getenv("THC_EDIT_CAPTURE");
    if (capture && *capture && !thc_capture(capture)) return 0;
    return SDL_RenderPresent(renderer);
}
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
static int keycode(SDL_Keycode key) {
    if (key >= SDLK_F1 && key <= SDLK_F12) return -101 - (int)(key - SDLK_F1);
    if (key >= SDLK_F13 && key <= SDLK_F24) return -113 - (int)(key - SDLK_F13);
    switch (key) {
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
    event[2] = (int)floor((y * pixel_h / SDL_max(1, wh) - origin_y) / (16 * scale));
    event[4] = modifiers(SDL_GetModState());
}
void thc_post_command(int command) {
    SDL_Event e; SDL_zero(e); e.type = command_event; e.user.code = command; SDL_PushEvent(&e);
}
int thc_wait(int32_t *out) {
    SDL_Event e;
    memset(out, 0, 6 * sizeof(*out));
    while (SDL_WaitEvent(&e)) {
        if (e.type == command_event) { out[0] = 11; out[1] = e.user.code; return 1; }
        switch (e.type) {
        case SDL_EVENT_QUIT: case SDL_EVENT_WINDOW_CLOSE_REQUESTED: out[0] = 6; return 1;
        case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED: case SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED:
            geometry(); out[0] = 5; out[1] = cols; out[2] = rows; return 1;
        case SDL_EVENT_WINDOW_EXPOSED: out[0] = 8; return 1;
        case SDL_EVENT_WINDOW_FOCUS_LOST:
            left_down = false; SDL_CaptureMouse(false); out[0] = 7; return 1;
        case SDL_EVENT_KEY_DOWN: {
            int key = keycode(e.key.key), mods = modifiers(e.key.mod);
            /* Printable unmodified keys arrive only through TEXT_INPUT (IME/layout aware). */
            if (key == INT_MIN || (key >= 0 && !(mods & 14))) break;
            if (key >= 0) {
#ifdef SDL_PLATFORM_MACOS
                /* Cocoa menus own Command shortcuts; Option composes text.
                 * Keep Control/WordStar and modified function/navigation keys. */
                if ((mods & 8) || ((mods & 4) && !(mods & 2))) break;
#else
                /* AltGr produces TEXT_INPUT, not Alt menu/Control shortcuts.
                 * Left Alt remains available for editor menu shortcuts. */
                if (e.key.mod & (SDL_KMOD_MODE | SDL_KMOD_RALT)) break;
#endif
            }
            out[0] = 1; out[1] = key; out[2] = mods; return 1;
        }
        case SDL_EVENT_TEXT_INPUT:
            SDL_free(input_text); input_text = SDL_strdup(e.text.text);
            if (!input_text) return 0;
            out[0] = 2; return 1;
        case SDL_EVENT_MOUSE_BUTTON_DOWN:
            if (e.button.button != SDL_BUTTON_LEFT) break;
            left_down = true; SDL_CaptureMouse(true);
            out[0] = 3; pointer(e.button.x, e.button.y, out); return 1;
        case SDL_EVENT_MOUSE_BUTTON_UP:
            if (e.button.button != SDL_BUTTON_LEFT) break;
            left_down = false; SDL_CaptureMouse(false);
            out[0] = 4; pointer(e.button.x, e.button.y, out); return 1;
        case SDL_EVENT_MOUSE_MOTION:
            if (!left_down) break;
            out[0] = 3; pointer(e.motion.x, e.motion.y, out); return 1;
        case SDL_EVENT_MOUSE_WHEEL:
            if (e.wheel.y == 0) break;
            out[0] = 9; pointer(e.wheel.mouse_x, e.wheel.mouse_y, out);
            out[3] = e.wheel.y > 0 ? 1 : -1;
            if (e.wheel.direction == SDL_MOUSEWHEEL_FLIPPED) out[3] = -out[3];
            return 1;
        default: break;
        }
    }
    return 0;
}
