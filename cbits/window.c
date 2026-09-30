#include "window.h"
#include <SDL3/SDL.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

static SDL_Window *window;
static SDL_Renderer *renderer;
static SDL_Texture *texture, *vignette;
static bool crt_filter;
static uint32_t *pixels;
static double scale;
static int cols, rows, cell_height, origin_x, origin_y, pixel_w, pixel_h;
static int mouse_x = -1, mouse_y = -1;
static char *input_text;
static char *clipboard_text;
static bool left_down;
static bool suppress_option_text;
static bool blink_cursor = true, cursor_present, cursor_drawn;
static int cursor_x = -1, cursor_y = -1;
static Uint64 cursor_epoch;
static Uint32 command_event;
static void pointer(float x, float y, int32_t *event);

static void clear_pointer(void) {
    mouse_x = mouse_y = -1;
    SDL_ShowCursor();
}

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
    clear_pointer();
    left_down = false;
    suppress_option_text = false;
    cursor_present = false; cursor_x = cursor_y = -1;
    SDL_StopTextInput(window);
    SDL_DestroyTexture(texture); texture = NULL;
    SDL_DestroyTexture(vignette); vignette = NULL;
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
    rows = SDL_clamp(pixel_h / (cell_height * scale), 1, 256);
    origin_x = (pixel_w - cols * 8 * scale) / 2;
    origin_y = (pixel_h - rows * cell_height * scale) / 2;
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
    if (!SDL_Init(SDL_INIT_VIDEO)) return 0;
    crt_filter = false;
    blink_cursor = true; cursor_epoch = SDL_GetTicks();
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

void thc_size(int *w, int *h) { geometry(); *w = cols; *h = rows; }
int thc_begin(void) {
    geometry();
    cursor_present = false;
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
static bool cursor_phase(void) {
    return !blink_cursor || ((SDL_GetTicks() - cursor_epoch) / 500) % 2 == 0;
}
void thc_cursor_blink(int enabled) {
    if (blink_cursor != (enabled != 0)) cursor_epoch = SDL_GetTicks();
    blink_cursor = enabled != 0;
}
void thc_cursor(int x, int y) {
    if (!pixels || x < 0 || x >= cols || y < 0 || y >= rows) return;
    if (cursor_x != x || cursor_y != y) cursor_epoch = SDL_GetTicks();
    cursor_x = x; cursor_y = y; cursor_present = true;
    cursor_drawn = cursor_phase();
    if (!cursor_drawn) return;
    for (int dy = 14; dy < 16; ++dy) for (int dx = 0; dx < 8; ++dx)
        pixels[(y * 16 + dy) * cols * 8 + x * 8 + dx] ^= 0x00ffffff;
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
int thc_present(void) {
    uint32_t saved_cell[128];
    bool hovered = mouse_x >= 0 && mouse_x < cols && mouse_y >= 0 && mouse_y < rows;
    if (hovered) for (int y = 0; y < 16; ++y) {
        uint32_t *row = pixels + (mouse_y * 16 + y) * cols * 8 + mouse_x * 8;
        memcpy(saved_cell + y * 8, row, 8 * sizeof(*row));
        for (int x = 0; x < 8; ++x) row[x] = mouse_color(row[x]);
    }
    bool uploaded = SDL_UpdateTexture(texture, NULL, pixels, cols * 8 * sizeof(*pixels));
    if (hovered) for (int y = 0; y < 16; ++y)
        memcpy(pixels + (mouse_y * 16 + y) * cols * 8 + mouse_x * 8, saved_cell + y * 8, 8 * sizeof(*pixels));
    if (!uploaded) return 0;
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    if (!SDL_RenderClear(renderer)) return 0;
    SDL_FRect target = {(float)origin_x, (float)origin_y, (float)(cols * 8 * scale), (float)(rows * cell_height * scale)};
    if (!SDL_RenderTexture(renderer, texture, NULL, &target)) return 0;
    if (crt_filter && !draw_crt(&target)) return 0;
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
void thc_post_command(int command) {
    SDL_Event e; SDL_zero(e); e.type = command_event; e.user.code = command; SDL_PushEvent(&e);
}
static int idle_event(int32_t *out) {
    out[0] = cursor_present && cursor_drawn != cursor_phase() ? 8 : 0;
    return 1;
}
int thc_wait(int32_t *out) {
    SDL_Event e;
    memset(out, 0, 6 * sizeof(*out));
    Uint64 deadline = SDL_GetTicks() + 100;
    for (;;) {
        Uint64 now = SDL_GetTicks();
        if (now >= deadline) return idle_event(out);
        SDL_ClearError();
        if (!SDL_WaitEventTimeout(&e, (Sint32)(deadline - now))) return *SDL_GetError() ? 0 : idle_event(out);
        if (e.type == command_event) { out[0] = 11; out[1] = e.user.code; return 1; }
        switch (e.type) {
        case SDL_EVENT_QUIT: case SDL_EVENT_WINDOW_CLOSE_REQUESTED: out[0] = 6; return 1;
        case SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED: case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
            geometry(); refresh_pointer(); out[0] = 5; out[1] = cols; out[2] = rows; return 1;
        case SDL_EVENT_WINDOW_EXPOSED: out[0] = 8; return 1;
        case SDL_EVENT_WINDOW_MOUSE_ENTER:
            refresh_pointer(); out[0] = 12; out[1] = mouse_x; out[2] = mouse_y; return 1;
        case SDL_EVENT_WINDOW_MOUSE_LEAVE:
            clear_pointer();
            out[0] = 12; out[1] = out[2] = -1; return 1;
        case SDL_EVENT_WINDOW_FOCUS_LOST:
            clear_pointer(); left_down = false; suppress_option_text = false;
            SDL_CaptureMouse(false); out[0] = 7; return 1;
        case SDL_EVENT_KEY_DOWN: {
            suppress_option_text = false;
            cursor_epoch = SDL_GetTicks();
            int key = keycode(e.key.key), mods = modifiers(e.key.mod);
            /* Printable unmodified keys arrive only through TEXT_INPUT (IME/layout aware). */
            if (key == INT_MIN || (key >= 0 && !(mods & 14))) break;
            if (key >= 0) {
#ifdef SDL_PLATFORM_MACOS
                /* Cocoa menus own Command shortcuts; Option composes text.
                 * Option digits and +/- are shortcuts, consuming only paired text. */
                if ((mods & 14) == 4 && ((key >= '0' && key <= '9') || key == '+' || key == '=' || key == '-')) suppress_option_text = true;
                else if ((mods & 8) || ((mods & 4) && !(mods & 2))) break;
#else
                /* AltGr produces TEXT_INPUT, not Alt menu/Control shortcuts.
                 * Left Alt remains available for editor menu shortcuts. */
                if (e.key.mod & (SDL_KMOD_MODE | SDL_KMOD_RALT)) break;
#endif
            }
            out[0] = 1; out[1] = key; out[2] = mods; return 1;
        }
        case SDL_EVENT_KEY_UP:
            suppress_option_text = false; break;
        case SDL_EVENT_TEXT_INPUT:
            if (suppress_option_text) { suppress_option_text = false; break; }
            cursor_epoch = SDL_GetTicks();
            SDL_free(input_text); input_text = SDL_strdup(e.text.text);
            if (!input_text) return 0;
            out[0] = 2; return 1;
        case SDL_EVENT_MOUSE_BUTTON_DOWN:
            if (e.button.button != SDL_BUTTON_LEFT && e.button.button != SDL_BUTTON_RIGHT) break;
            if (e.button.button == SDL_BUTTON_LEFT) { left_down = true; SDL_CaptureMouse(true); }
            out[0] = 3; out[3] = e.button.clicks; out[5] = e.button.button;
            pointer(e.button.x, e.button.y, out); return 1;
        case SDL_EVENT_MOUSE_BUTTON_UP:
            if (e.button.button != SDL_BUTTON_LEFT) break;
            left_down = false; SDL_CaptureMouse(false);
            out[0] = 4; pointer(e.button.x, e.button.y, out); return 1;
        case SDL_EVENT_MOUSE_MOTION: {
            int old_x = mouse_x, old_y = mouse_y;
            bool was_visible = SDL_CursorVisible();
            pointer(e.motion.x, e.motion.y, out);
            if (!left_down && mouse_x == old_x && mouse_y == old_y && SDL_CursorVisible() == was_visible) break;
            out[0] = left_down ? 3 : 12; return 1;
        }
        case SDL_EVENT_MOUSE_WHEEL:
            if (e.wheel.y == 0) break;
            out[0] = 9; pointer(e.wheel.mouse_x, e.wheel.mouse_y, out);
            out[3] = e.wheel.y > 0 ? 1 : -1;
            if (e.wheel.direction == SDL_MOUSEWHEEL_FLIPPED) out[3] = -out[3];
            return 1;
        default: break;
        }
    }
}
