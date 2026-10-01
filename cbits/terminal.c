/* libghostty-vt API pinned to 76895d97b74ff6b24c2b1543bcd69ccc18048a4d. */
#include "terminal.h"
#include <ghostty/vt.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __APPLE__
#include <util.h>
#else
#include <pty.h>
#endif

#define OUTPUT_LIMIT (256 * 1024)
#define INPUT_LIMIT (1024 * 1024)
struct thc_terminal {
    GhosttyTerminal terminal;
    GhosttyRenderState render;
    GhosttyRenderStateRowIterator row;
    GhosttyRenderStateRowCells cell;
    int columns, rows, master, exited, exit_code, drained, reaped;
    pid_t pid;
    uint32_t *cells;
    uint8_t *text, *input;
    size_t text_length, text_capacity, input_length;
    uint8_t output[OUTPUT_LIMIT];
    size_t output_length;
    int cursor_x, cursor_y;
    int appearance;
    char error[256];
};

static int fail(thc_terminal *t, const char *what) {
    snprintf(t->error, sizeof(t->error), "%s: %s", what, strerror(errno));
    return 0;
}
static int dimensions(int columns, int rows) {
    return columns > 0 && rows > 0 && columns <= 1000 && rows <= 1000;
}
static int vt_check(thc_terminal *t, GhosttyResult result) {
    if (result == GHOSTTY_SUCCESS) return 1;
    snprintf(t->error, sizeof(t->error), "libghostty-vt error %d", (int)result);
    return 0;
}
int thc_terminal_appearance(thc_terminal *t, int dark) {
    int appearance = dark ? 2 : 1;
    if (t->appearance == appearance) return 1;
    GhosttyColorRgb fg = dark ? (GhosttyColorRgb){255,255,255} : (GhosttyColorRgb){0,0,0};
    GhosttyColorRgb bg = dark ? (GhosttyColorRgb){0,0,0} : (GhosttyColorRgb){170,170,170};
    if (!vt_check(t, ghostty_terminal_set(t->terminal, GHOSTTY_TERMINAL_OPT_COLOR_FOREGROUND, &fg)) ||
        !vt_check(t, ghostty_terminal_set(t->terminal, GHOSTTY_TERMINAL_OPT_COLOR_BACKGROUND, &bg))) return 0;
    t->appearance = appearance;
    return 1;
}
static int flush_input(thc_terminal *t) {
    while (t->master >= 0 && t->input_length) {
        ssize_t n = write(t->master, t->input, t->input_length);
        if (n > 0) {
            t->input_length -= (size_t)n;
            memmove(t->input, t->input + n, t->input_length);
        } else if (n < 0 && errno == EINTR) continue;
        else if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) break;
        else return fail(t, "write PTY");
    }
    return 1;
}
int thc_terminal_write(thc_terminal *t, const uint8_t *data, size_t length) {
    if (t->master < 0 || t->exited) { errno = EPIPE; return fail(t, "terminal input"); }
    if (!flush_input(t)) return 0;
    if (length > INPUT_LIMIT - t->input_length) { errno = ENOBUFS; return fail(t, "terminal input queue"); }
    if (!length) return 1;
    uint8_t *p = realloc(t->input, t->input_length + length);
    if (!p) return fail(t, "terminal input allocation");
    t->input = p;
    memcpy(p + t->input_length, data, length);
    t->input_length += length;
    return flush_input(t);
}
static void pty_reply(GhosttyTerminal terminal, void *userdata, const uint8_t *data, size_t length) {
    (void)terminal;
    thc_terminal *t = userdata;
    if (t->master >= 0) (void)thc_terminal_write(t, data, length);
}
thc_terminal *thc_terminal_new(int columns, int rows) {
    if (!dimensions(columns, rows)) { errno = EINVAL; return NULL; }
    thc_terminal *t = calloc(1, sizeof(*t));
    if (!t) return NULL;
    t->master = -1;
    t->columns = columns;
    t->rows = rows;
    t->cursor_x = t->cursor_y = -1;
    if (ghostty_terminal_new(NULL, &t->terminal, columns, rows) != GHOSTTY_SUCCESS ||
        ghostty_render_state_new(NULL, &t->render) != GHOSTTY_SUCCESS ||
        ghostty_render_state_row_iterator_new(NULL, &t->row) != GHOSTTY_SUCCESS ||
        ghostty_render_state_row_cells_new(NULL, &t->cell) != GHOSTTY_SUCCESS) {
        thc_terminal_free(t);
        errno = ENOMEM;
        return NULL;
    }
    ghostty_terminal_set(t->terminal, GHOSTTY_TERMINAL_OPT_USERDATA, t);
    ghostty_terminal_set(t->terminal, GHOSTTY_TERMINAL_OPT_WRITE_PTY, (const void *)pty_reply);
    return t;
}
int thc_terminal_spawn(thc_terminal *t, const char *executable, char *const argv[],
                       char *const env[], const char *directory) {
    if (t->pid || t->master >= 0) { errno = EINVAL; return fail(t, "terminal already started"); }
    int errors[2];
    if (pipe(errors)) return fail(t, "exec error pipe");
    fcntl(errors[0], F_SETFD, FD_CLOEXEC);
    fcntl(errors[1], F_SETFD, FD_CLOEXEC);
    struct winsize size = {.ws_row = (unsigned short)t->rows, .ws_col = (unsigned short)t->columns};
    int maxfd = getdtablesize();
    pid_t pid = forkpty(&t->master, NULL, NULL, &size);
    if (pid < 0) {
        int saved = errno;
        close(errors[0]); close(errors[1]);
        errno = saved;
        return fail(t, "forkpty");
    }
    if (!pid) {
        close(errors[0]);
        /* Only async-signal-safe operations after fork from the Haskell runtime. */
        for (int fd = 3; fd < maxfd; ++fd) if (fd != errors[1]) close(fd);
        sigset_t mask;
        sigemptyset(&mask);
        sigprocmask(SIG_SETMASK, &mask, NULL);
        struct sigaction action = {.sa_handler = SIG_DFL};
        sigemptyset(&action.sa_mask);
        sigaction(SIGPIPE, &action, NULL);
        sigaction(SIGINT, &action, NULL);
        sigaction(SIGQUIT, &action, NULL);
        sigaction(SIGCHLD, &action, NULL);
        if (!chdir(directory)) execve(executable, argv, env);
        int saved = errno;
        (void)write(errors[1], &saved, sizeof(saved));
        _exit(127);
    }
    t->pid = pid;
    close(errors[1]);
    int saved = 0;
    ssize_t n;
    do { n = read(errors[0], &saved, sizeof(saved)); } while (n < 0 && errno == EINTR);
    close(errors[0]);
    if (n > 0) {
        thc_terminal_kill(t);
        errno = saved;
        return fail(t, "start terminal command");
    }
    if (fcntl(t->master, F_SETFL, O_NONBLOCK) < 0 || fcntl(t->master, F_SETFD, FD_CLOEXEC) < 0) {
        thc_terminal_kill(t);
        return fail(t, "configure PTY");
    }
    return 1;
}
void thc_terminal_feed(thc_terminal *t, const uint8_t *data, size_t length) {
    ghostty_terminal_vt_write(t->terminal, data, length);
}
int thc_terminal_resize(thc_terminal *t, int columns, int rows) {
    if (!dimensions(columns, rows)) { errno = EINVAL; return fail(t, "terminal size"); }
    if (!vt_check(t, ghostty_terminal_resize(t->terminal, columns, rows, 8, 16))) return 0;
    t->columns = columns; t->rows = rows;
    if (t->master >= 0) {
        struct winsize size = {.ws_row = (unsigned short)rows, .ws_col = (unsigned short)columns};
        if (ioctl(t->master, TIOCSWINSZ, &size)) return fail(t, "resize PTY");
    }
    return 1;
}
static uint32_t rgb(GhosttyColorRgb c) { return (uint32_t)c.r << 16 | (uint32_t)c.g << 8 | c.b; }
static int snapshot(thc_terminal *t) {
    if (!vt_check(t, ghostty_render_state_update(t->render, t->terminal))) return 0;
    GhosttyRenderStateColors colors = GHOSTTY_INIT_SIZED(GhosttyRenderStateColors);
    GhosttyRenderStateCursor cursor = GHOSTTY_INIT_SIZED(GhosttyRenderStateCursor);
    if (!vt_check(t, ghostty_render_state_get(t->render, GHOSTTY_RENDER_STATE_DATA_COLORS, &colors)) ||
        !vt_check(t, ghostty_render_state_get(t->render, GHOSTTY_RENDER_STATE_DATA_CURSOR, &cursor)) ||
        !vt_check(t, ghostty_render_state_get(t->render, GHOSTTY_RENDER_STATE_DATA_ROW_ITERATOR, &t->row))) return 0;
    t->cursor_x = t->cursor_y = -1;
    if (cursor.visible && cursor.viewport_has_value) {
        t->cursor_x = cursor.viewport_x; t->cursor_y = cursor.viewport_y;
    }
    size_t count = (size_t)t->columns * t->rows;
    uint32_t *cells = realloc(t->cells, count * 6 * sizeof(uint32_t));
    if (!cells) return fail(t, "cell allocation");
    t->cells = cells;
    memset(cells, 0, count * 6 * sizeof(uint32_t));
    t->text_length = 0;
    size_t index = 0;
    while (ghostty_render_state_row_iterator_next(t->row)) {
        if (!vt_check(t, ghostty_render_state_row_get(t->row, GHOSTTY_RENDER_STATE_ROW_DATA_CELLS, &t->cell))) return 0;
        while (ghostty_render_state_row_cells_next(t->cell) && index < count) {
            GhosttyStyle style = GHOSTTY_INIT_SIZED(GhosttyStyle);
            GhosttyCell raw;
            GhosttyCellWide wide;
            GhosttyColorRgb fg = colors.foreground, bg = colors.background;
            ghostty_render_state_row_cells_get(t->cell, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_STYLE, &style);
            ghostty_render_state_row_cells_get(t->cell, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_RAW, &raw);
            ghostty_cell_get(raw, GHOSTTY_CELL_DATA_WIDE, &wide);
            ghostty_render_state_row_cells_get(t->cell, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_FG_COLOR, &fg);
            ghostty_render_state_row_cells_get(t->cell, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_BG_COLOR, &bg);
            if (style.inverse) { GhosttyColorRgb temp = fg; fg = bg; bg = temp; }
            if (style.invisible) fg = bg;
            GhosttyBuffer buf = {0};
            GhosttyResult result = ghostty_render_state_row_cells_get(t->cell, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_UTF8, &buf);
            if (result != GHOSTTY_SUCCESS && result != GHOSTTY_OUT_OF_SPACE) return vt_check(t, result);
            size_t length = buf.len;
            if (length > UINT32_MAX - t->text_length) { errno = EOVERFLOW; return fail(t, "terminal text"); }
            if (t->text_length + length > t->text_capacity) {
                size_t capacity = (t->text_length + length) * 2 + 4096;
                uint8_t *p = realloc(t->text, capacity);
                if (!p) return fail(t, "text allocation");
                t->text = p; t->text_capacity = capacity;
            }
            if (length) {
                buf.ptr = t->text + t->text_length; buf.cap = length;
                if (!vt_check(t, ghostty_render_state_row_cells_get(t->cell, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_UTF8, &buf))) return 0;
            }
            uint32_t *out = cells + index++ * 6;
            out[0] = (uint32_t)t->text_length; out[1] = (uint32_t)length;
            out[2] = rgb(fg); out[3] = rgb(bg);
            out[4] = (style.bold ? 1 : 0) | (style.italic ? 2 : 0) | (style.underline ? 4 : 0) |
                     (style.strikethrough ? 8 : 0) | (style.faint ? 16 : 0);
            out[5] = wide == GHOSTTY_CELL_WIDE_WIDE ? 2 : wide == GHOSTTY_CELL_WIDE_NARROW ? 1 : 0;
            t->text_length += length;
        }
    }
    return vt_check(t, ghostty_render_state_clean(t->render));
}
static int read_output(thc_terminal *t) {
    while (t->master >= 0 && t->output_length < OUTPUT_LIMIT) {
        ssize_t n = read(t->master, t->output + t->output_length, OUTPUT_LIMIT - t->output_length);
        if (n > 0) {
            thc_terminal_feed(t, t->output + t->output_length, (size_t)n);
            t->output_length += (size_t)n;
        } else if (n < 0 && errno == EINTR) continue;
        else if (n == 0 || (n < 0 && errno == EIO)) { close(t->master); t->master = -1; break; }
        else if (errno == EAGAIN || errno == EWOULDBLOCK) break;
        else return fail(t, "read PTY");
    }
    if (t->output_length == OUTPUT_LIMIT) t->drained = 0;
    return 1;
}
int thc_terminal_poll(thc_terminal *t) {
    t->output_length = 0;
    t->drained = 1;
    if (!flush_input(t)) {
        if (errno == EIO || errno == EPIPE) t->input_length = 0;
        else return 0;
    }
    if (!read_output(t)) return 0;
    if (t->pid > 0 && !t->exited) {
        siginfo_t status = {0};
        int result = waitid(P_PID, (id_t)t->pid, &status, WEXITED | WNOHANG | WNOWAIT);
        if (!result && status.si_pid) {
            /* Keep the child waitable until release so its process group ID
             * cannot be recycled before descendant cleanup. */
            t->exited = 1;
            t->exit_code = status.si_code == CLD_EXITED ? status.si_status : 128 + status.si_status;
            /* Drain bytes written between the first read and process exit. */
            if (!read_output(t)) return 0;
        } else if (result < 0 && errno != EINTR) return fail(t, "wait terminal process");
    }
    return snapshot(t);
}
const uint32_t *thc_terminal_cells(thc_terminal *t) { return t->cells; }
const uint8_t *thc_terminal_text(thc_terminal *t, size_t *length) { *length = t->text_length; return t->text; }
const uint8_t *thc_terminal_output(thc_terminal *t, size_t *length) { *length = t->output_length; return t->output; }
void thc_terminal_info(thc_terminal *t, int info[6]) {
    info[0] = t->columns; info[1] = t->rows; info[2] = t->cursor_x; info[3] = t->cursor_y;
    info[4] = t->exited && t->drained; info[5] = t->exit_code;
}
const char *thc_terminal_error(thc_terminal *t) { return t->error; }
void thc_terminal_kill(thc_terminal *t) {
    if (t->pid > 0 && !t->reaped) {
        /* Interactive shells put foreground jobs in a separate process group.
         * Read it before terminating the shell while this private PTY owns it. */
        pid_t foreground = t->master >= 0 ? tcgetpgrp(t->master) : -1;
        if (foreground > 0 && foreground != t->pid && foreground != getpgrp())
            kill(-foreground, SIGKILL);
        /* forkpty creates a session/process group for the command. */
        kill(-t->pid, SIGKILL);
        kill(t->pid, SIGKILL);
        int status;
        pid_t pid;
        do { pid = waitpid(t->pid, &status, 0); } while (pid < 0 && errno == EINTR);
        t->exited = 1;
        t->reaped = 1;
        t->exit_code = pid > 0 ? (WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status)) : 137;
    }
}
void thc_terminal_free(thc_terminal *t) {
    if (!t) return;
    thc_terminal_kill(t);
    if (t->master >= 0) close(t->master);
    if (t->cell) ghostty_render_state_row_cells_free(t->cell);
    if (t->row) ghostty_render_state_row_iterator_free(t->row);
    if (t->render) ghostty_render_state_free(t->render);
    if (t->terminal) ghostty_terminal_free(t->terminal);
    free(t->cells); free(t->text); free(t->input); free(t);
}
