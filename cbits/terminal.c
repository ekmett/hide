/* libghostty-vt API pinned to 76895d97b74ff6b24c2b1543bcd69ccc18048a4d. */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0A00
#endif
#ifndef NTDDI_VERSION
#define NTDDI_VERSION 0x0A000006 /* Windows 10 1809: ConPTY declarations. */
#endif
#endif
#include "terminal.h"
#include <ghostty/vt.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
typedef HRESULT (WINAPI *CreateConsoleFn)(COORD, HANDLE, HANDLE, DWORD, HPCON *);
typedef HRESULT (WINAPI *ResizeConsoleFn)(HPCON, COORD);
typedef void (WINAPI *CloseConsoleFn)(HPCON);
#else
#include <fcntl.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __APPLE__
#include <util.h>
#else
#include <pty.h>
#endif
#endif

#define OUTPUT_LIMIT (256 * 1024)
#define INPUT_LIMIT (1024 * 1024)
struct thc_terminal {
    GhosttyTerminal terminal;
    GhosttyRenderState render;
    GhosttyRenderStateRowIterator row;
    GhosttyRenderStateRowCells cell;
    GhosttyMouseEncoder mouse_encoder;
    GhosttyMouseEvent mouse_event;
    int columns, rows, master, exited, exit_code, drained, reaped, preserve_output;
#ifdef _WIN32
    HPCON console, closing_console;
    HANDLE child, job, input_pipe, output_pipe, reader, writer, closer;
    CreateConsoleFn create_console;
    ResizeConsoleFn resize_console;
    CloseConsoleFn close_console;
    CRITICAL_SECTION io_lock;
    CONDITION_VARIABLE input_ready, output_space;
    uint8_t pending_output[OUTPUT_LIMIT];
    size_t pending_length;
    DWORD io_error;
    int stopping, input_stopped, output_eof;
#else
    pid_t pid;
#endif
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
#ifdef _WIN32
static int win_fail(thc_terminal *t, const char *what, DWORD error) {
    snprintf(t->error, sizeof(t->error), "%s: Windows error %lu", what, (unsigned long)error);
    return 0;
}
static DWORD WINAPI read_pipe(void *context) {
    thc_terminal *t = context;
    uint8_t bytes[4096];
    for (;;) {
        EnterCriticalSection(&t->io_lock);
        while (!t->stopping && OUTPUT_LIMIT - t->pending_length < sizeof(bytes))
            SleepConditionVariableCS(&t->output_space, &t->io_lock, INFINITE);
        int stopping = t->stopping;
        LeaveCriticalSection(&t->io_lock);
        if (stopping) break;
        DWORD count = 0;
        BOOL ok = ReadFile(t->output_pipe, bytes, sizeof(bytes), &count, NULL);
        DWORD error = ok ? 0 : GetLastError();
        EnterCriticalSection(&t->io_lock);
        if (count) {
            memcpy(t->pending_output + t->pending_length, bytes, count);
            t->pending_length += count;
        }
        if (!ok || !count) {
            t->output_eof = 1;
            if (!t->stopping && error != ERROR_BROKEN_PIPE && error != ERROR_NO_DATA)
                t->io_error = error;
        }
        LeaveCriticalSection(&t->io_lock);
        if (!ok || !count) break;
    }
    return 0;
}
static DWORD WINAPI write_pipe(void *context) {
    thc_terminal *t = context;
    uint8_t bytes[4096];
    for (;;) {
        EnterCriticalSection(&t->io_lock);
        while (!t->stopping && !t->input_stopped && !t->input_length)
            SleepConditionVariableCS(&t->input_ready, &t->io_lock, INFINITE);
        int stopping = t->stopping || t->input_stopped;
        DWORD count = (DWORD)(t->input_length < sizeof(bytes) ? t->input_length : sizeof(bytes));
        if (!stopping) memcpy(bytes, t->input, count);
        LeaveCriticalSection(&t->io_lock);
        if (stopping) break;
        DWORD written = 0;
        BOOL ok = WriteFile(t->input_pipe, bytes, count, &written, NULL);
        DWORD error = ok ? 0 : GetLastError();
        EnterCriticalSection(&t->io_lock);
        if (written) {
            t->input_length -= written;
            memmove(t->input, t->input + written, t->input_length);
        }
        if (!ok) {
            t->input_stopped = 1;
            if (!t->stopping && error != ERROR_BROKEN_PIPE && error != ERROR_NO_DATA && error != ERROR_OPERATION_ABORTED)
                t->io_error = error;
        }
        LeaveCriticalSection(&t->io_lock);
        if (!ok) break;
    }
    return 0;
}
static DWORD WINAPI close_console(void *context) {
    thc_terminal *t = context;
    t->close_console(t->closing_console);
    return 0;
}
int thc_terminal_write(thc_terminal *t, const uint8_t *data, size_t length) {
    EnterCriticalSection(&t->io_lock);
    DWORD error = 0;
    if (!t->child || t->exited || t->input_stopped || t->stopping) error = ERROR_BROKEN_PIPE;
    else if (length > INPUT_LIMIT - t->input_length) error = ERROR_NOT_ENOUGH_QUOTA;
    else if (length) {
        uint8_t *input = realloc(t->input, t->input_length + length);
        if (!input) error = ERROR_NOT_ENOUGH_MEMORY;
        else {
            t->input = input;
            memcpy(t->input + t->input_length, data, length);
            t->input_length += length;
            WakeConditionVariable(&t->input_ready);
        }
    }
    LeaveCriticalSection(&t->io_lock);
    return error ? win_fail(t, "terminal input queue", error) : 1;
}
int thc_terminal_spawn_windows(thc_terminal *t, const wchar_t *executable, wchar_t *command,
                               const wchar_t *environment, const wchar_t *directory) {
    if (t->child || t->console) return win_fail(t, "terminal already started", ERROR_INVALID_PARAMETER);
    HMODULE kernel = GetModuleHandleW(L"kernel32.dll");
    t->create_console = (CreateConsoleFn)(void *)GetProcAddress(kernel, "CreatePseudoConsole");
    t->resize_console = (ResizeConsoleFn)(void *)GetProcAddress(kernel, "ResizePseudoConsole");
    t->close_console = (CloseConsoleFn)(void *)GetProcAddress(kernel, "ClosePseudoConsole");
    if (!t->create_console || !t->resize_console || !t->close_console)
        return win_fail(t, "ConPTY requires Windows 10 version 1809 or newer", ERROR_CALL_NOT_IMPLEMENTED);
    HANDLE input_read = NULL, output_write = NULL;
    STARTUPINFOEXW startup = {0};
    PROCESS_INFORMATION child = {0};
    SIZE_T bytes = 0;
    int attributes_ready = 0;
    DWORD error = 0;
    t->job = CreateJobObjectW(NULL, NULL);
    if (!t->job) { error = GetLastError(); goto done; }
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits = {0};
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(t->job, JobObjectExtendedLimitInformation, &limits, sizeof(limits))) { error = GetLastError(); goto done; }
    if (!CreatePipe(&input_read, &t->input_pipe, NULL, 0) ||
        !CreatePipe(&t->output_pipe, &output_write, NULL, 0)) { error = GetLastError(); goto done; }
    HRESULT hr = t->create_console((COORD){(SHORT)t->columns, (SHORT)t->rows}, input_read, output_write, 0, &t->console);
    if (FAILED(hr)) { error = (DWORD)hr; goto done; }
    /* SSH can leave the host ignoring Ctrl-C, which Windows propagates to
     * new children. Reenable delivery without removing installed handlers. */
    if (!SetConsoleCtrlHandler(NULL, FALSE)) { error = GetLastError(); goto done; }
    InitializeProcThreadAttributeList(NULL, 1, 0, &bytes);
    startup.lpAttributeList = malloc(bytes);
    if (!startup.lpAttributeList) { error = ERROR_NOT_ENOUGH_MEMORY; goto done; }
    if (!InitializeProcThreadAttributeList(startup.lpAttributeList, 1, 0, &bytes)) { error = GetLastError(); goto done; }
    attributes_ready = 1;
    if (!UpdateProcThreadAttribute(startup.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
                                   t->console, sizeof(t->console), NULL, NULL)) { error = GetLastError(); goto done; }
    startup.StartupInfo.cb = sizeof(startup);
    /* Null explicit handles prevent redirected daemon stdio from bypassing
     * the pseudoconsole through Windows standard-handle inheritance. */
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    if (!CreateProcessW(executable, command, NULL, NULL, FALSE,
                        EXTENDED_STARTUPINFO_PRESENT | CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT,
                        (void *)environment, directory, &startup.StartupInfo, &child)) { error = GetLastError(); goto done; }
    t->child = child.hProcess;
    if (!AssignProcessToJobObject(t->job, t->child)) { error = GetLastError(); TerminateProcess(t->child, 137); goto done; }
    t->reader = CreateThread(NULL, 0, read_pipe, t, 0, NULL);
    if (!t->reader) { error = GetLastError(); goto done; }
    t->writer = CreateThread(NULL, 0, write_pipe, t, 0, NULL);
    if (!t->writer) { error = GetLastError(); goto done; }
    if (ResumeThread(child.hThread) == (DWORD)-1) error = GetLastError();
done:
    if (child.hThread) CloseHandle(child.hThread);
    if (attributes_ready) DeleteProcThreadAttributeList(startup.lpAttributeList);
    free(startup.lpAttributeList);
    if (input_read) CloseHandle(input_read);
    if (output_write) CloseHandle(output_write);
    return error ? win_fail(t, "start terminal command", error) : 1;
}
static int poll_process(thc_terminal *t) {
    EnterCriticalSection(&t->io_lock);
    t->output_length = t->pending_length;
    memcpy(t->output, t->pending_output, t->pending_length);
    t->pending_length = 0;
    t->drained = t->output_eof;
    DWORD error = t->io_error;
    WakeConditionVariable(&t->output_space);
    LeaveCriticalSection(&t->io_lock);
    if (error) return win_fail(t, "terminal pipe", error);
    if (t->output_length) thc_terminal_feed(t, t->output, t->output_length);
    if (t->child && !t->exited && WaitForSingleObject(t->child, 0) == WAIT_OBJECT_0) {
        DWORD code;
        if (!GetExitCodeProcess(t->child, &code)) return win_fail(t, "terminal exit code", GetLastError());
        t->exit_code = (int)code;
        /* Closing may emit a final frame and block on older Windows. The reader
         * keeps draining while subsequent polls consume its bounded queue. */
        t->closing_console = t->console;
        t->closer = CreateThread(NULL, 0, close_console, t, 0, NULL);
        if (!t->closer) return win_fail(t, "close terminal console", GetLastError());
        t->console = NULL;
        t->exited = 1;
    }
    return 1;
}
void thc_terminal_kill(thc_terminal *t) {
    if (t->job) TerminateJobObject(t->job, 137);
    EnterCriticalSection(&t->io_lock);
    t->input_stopped = 1;
    WakeConditionVariable(&t->input_ready);
    LeaveCriticalSection(&t->io_lock);
    if (t->writer) CancelSynchronousIo(t->writer);
}
static void join_io(HANDLE thread) {
    if (!thread) return;
    /* Repeat cancellation to cover a worker entering ReadFile/WriteFile just
     * after the stop flag was set and the first cancellation found no IO. */
    while (WaitForSingleObject(thread, 10) == WAIT_TIMEOUT) CancelSynchronousIo(thread);
    CloseHandle(thread);
}
static void free_process(thc_terminal *t) {
    thc_terminal_kill(t);
    EnterCriticalSection(&t->io_lock);
    t->stopping = 1;
    WakeAllConditionVariable(&t->input_ready);
    WakeAllConditionVariable(&t->output_space);
    LeaveCriticalSection(&t->io_lock);
    join_io(t->reader); join_io(t->writer);
    if (t->input_pipe) CloseHandle(t->input_pipe);
    /* No reader remains: close our output end before joining a potentially
     * blocking ClosePseudoConsole, so its final frame cannot deadlock cleanup. */
    if (t->output_pipe) CloseHandle(t->output_pipe);
    if (t->console) t->close_console(t->console);
    if (t->closer) { WaitForSingleObject(t->closer, INFINITE); CloseHandle(t->closer); }
    if (t->child) CloseHandle(t->child);
    if (t->job) CloseHandle(t->job);
    DeleteCriticalSection(&t->io_lock);
}
#endif

#ifndef _WIN32
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
#endif
static void pty_reply(GhosttyTerminal terminal, void *userdata, const uint8_t *data, size_t length) {
    (void)terminal;
    thc_terminal *t = userdata;
    (void)thc_terminal_write(t, data, length);
}
thc_terminal *thc_terminal_new(int columns, int rows) {
    if (!dimensions(columns, rows)) { errno = EINVAL; return NULL; }
    thc_terminal *t = calloc(1, sizeof(*t));
    if (!t) return NULL;
    t->master = -1;
#ifdef _WIN32
    InitializeCriticalSection(&t->io_lock);
    InitializeConditionVariable(&t->input_ready);
    InitializeConditionVariable(&t->output_space);
#endif
    t->columns = columns;
    t->rows = rows;
    t->cursor_x = t->cursor_y = -1;
    if (ghostty_terminal_new(NULL, &t->terminal, columns, rows) != GHOSTTY_SUCCESS ||
        ghostty_render_state_new(NULL, &t->render) != GHOSTTY_SUCCESS ||
        ghostty_render_state_row_iterator_new(NULL, &t->row) != GHOSTTY_SUCCESS ||
        ghostty_render_state_row_cells_new(NULL, &t->cell) != GHOSTTY_SUCCESS ||
        ghostty_mouse_encoder_new(NULL, &t->mouse_encoder) != GHOSTTY_SUCCESS ||
        ghostty_mouse_event_new(NULL, &t->mouse_event) != GHOSTTY_SUCCESS) {
        thc_terminal_free(t);
        errno = ENOMEM;
        return NULL;
    }
    ghostty_terminal_set(t->terminal, GHOSTTY_TERMINAL_OPT_USERDATA, t);
    ghostty_terminal_set(t->terminal, GHOSTTY_TERMINAL_OPT_WRITE_PTY, (const void *)pty_reply);
    return t;
}
#ifndef _WIN32
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
#endif
void thc_terminal_feed(thc_terminal *t, const uint8_t *data, size_t length) {
    ghostty_terminal_vt_write(t->terminal, data, length);
}
int thc_terminal_mouse_tracking(thc_terminal *t) {
    bool tracking = false;
    if (!vt_check(t, ghostty_terminal_get(t->terminal, GHOSTTY_TERMINAL_DATA_MOUSE_TRACKING, &tracking))) return -1;
    return tracking;
}
int thc_terminal_mouse(thc_terminal *t, int action, int button, int modifiers, int column, int row) {
    if (action < THC_TERMINAL_MOUSE_PRESS || action > THC_TERMINAL_MOUSE_MOTION ||
        button < THC_TERMINAL_MOUSE_NONE || button > THC_TERMINAL_MOUSE_WHEEL_RIGHT ||
        modifiers < 0 || modifiers > 7 ||
        (button == THC_TERMINAL_MOUSE_NONE && action != THC_TERMINAL_MOUSE_MOTION) ||
        (button >= THC_TERMINAL_MOUSE_WHEEL_UP && action != THC_TERMINAL_MOUSE_PRESS) ||
        (action == THC_TERMINAL_MOUSE_PRESS &&
            (column < 0 || row < 0 || column >= t->columns || row >= t->rows))) {
        errno = EINVAL; return fail(t, "terminal mouse event");
    }
    /* Cells are all the host supplies. Use the same logical 8x16 geometry
     * as terminal resize; no renderer pixel coordinates cross this boundary. */
    GhosttyMouseEncoderSize size = GHOSTTY_INIT_SIZED(GhosttyMouseEncoderSize);
    size.screen_width = (uint32_t)t->columns * 8; size.screen_height = (uint32_t)t->rows * 16;
    size.cell_width = 8; size.cell_height = 16;
    bool pressed = action != THC_TERMINAL_MOUSE_RELEASE &&
                   button >= THC_TERMINAL_MOUSE_LEFT && button <= THC_TERMINAL_MOUSE_RIGHT;
    ghostty_mouse_encoder_setopt_from_terminal(t->mouse_encoder, t->terminal);
    ghostty_mouse_encoder_setopt(t->mouse_encoder, GHOSTTY_MOUSE_ENCODER_OPT_SIZE, &size);
    ghostty_mouse_encoder_setopt(t->mouse_encoder, GHOSTTY_MOUSE_ENCODER_OPT_ANY_BUTTON_PRESSED, &pressed);
    ghostty_mouse_event_set_action(t->mouse_event, (GhosttyMouseAction)action);
    const GhosttyMouseButton buttons[] = {GHOSTTY_MOUSE_BUTTON_UNKNOWN, GHOSTTY_MOUSE_BUTTON_LEFT,
        GHOSTTY_MOUSE_BUTTON_MIDDLE, GHOSTTY_MOUSE_BUTTON_RIGHT, GHOSTTY_MOUSE_BUTTON_FOUR,
        GHOSTTY_MOUSE_BUTTON_FIVE, GHOSTTY_MOUSE_BUTTON_SIX, GHOSTTY_MOUSE_BUTTON_SEVEN};
    if (button == THC_TERMINAL_MOUSE_NONE) ghostty_mouse_event_clear_button(t->mouse_event);
    else ghostty_mouse_event_set_button(t->mouse_event, buttons[button]);
    GhosttyMods mods = (modifiers & 1 ? GHOSTTY_MODS_SHIFT : 0) |
                       (modifiers & 2 ? GHOSTTY_MODS_CTRL : 0) |
                       (modifiers & 4 ? GHOSTTY_MODS_ALT : 0);
    ghostty_mouse_event_set_mods(t->mouse_event, mods);
    /* Keep an out-of-grid position outside, while bounding conversion even
     * for maliciously large coordinates. Ghostty clamps cell protocols. */
    column = column < -1 ? -1 : column > t->columns ? t->columns : column;
    row = row < -1 ? -1 : row > t->rows ? t->rows : row;
    ghostty_mouse_event_set_position(t->mouse_event, (GhosttyMousePosition){column * 8.0f + 4, row * 16.0f + 8});
    char bytes[128]; size_t length = 0;
    if (!vt_check(t, ghostty_mouse_encoder_encode(t->mouse_encoder, t->mouse_event, bytes, sizeof(bytes), &length))) return 0;
    return length ? thc_terminal_write(t, (const uint8_t *)bytes, length) : 1;
}
int thc_terminal_resize(thc_terminal *t, int columns, int rows) {
    if (!dimensions(columns, rows)) { errno = EINVAL; return fail(t, "terminal size"); }
    if (!vt_check(t, ghostty_terminal_resize(t->terminal, columns, rows, 8, 16))) return 0;
    t->columns = columns; t->rows = rows;
    ghostty_mouse_encoder_reset(t->mouse_encoder);
#ifdef _WIN32
    if (t->console) {
        HRESULT hr = t->resize_console(t->console, (COORD){(SHORT)columns, (SHORT)rows});
        if (FAILED(hr)) return win_fail(t, "resize ConPTY", (DWORD)hr);
    }
#else
    if (t->master >= 0) {
        struct winsize size = {.ws_row = (unsigned short)rows, .ws_col = (unsigned short)columns};
        if (ioctl(t->master, TIOCSWINSZ, &size)) return fail(t, "resize PTY");
    }
#endif
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
#ifndef _WIN32
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
#endif
int thc_terminal_poll(thc_terminal *t) {
#ifdef _WIN32
    if (!poll_process(t)) return 0;
#else
    if (!t->preserve_output) t->output_length = 0;
    t->preserve_output = 0;
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
#endif
    return snapshot(t);
}
const uint32_t *thc_terminal_cells(thc_terminal *t) { return t->cells; }
const uint8_t *thc_terminal_text(thc_terminal *t, size_t *length) { *length = t->text_length; return t->text; }
const uint8_t *thc_terminal_output(thc_terminal *t, size_t *length) { *length = t->output_length; return t->output; }
int thc_terminal_pid(thc_terminal *t) {
#ifdef _WIN32
    return t->child ? (int)GetProcessId(t->child) : 0;
#else
    return (int)t->pid;
#endif
}
void thc_terminal_info(thc_terminal *t, int info[6]) {
    info[0] = t->columns; info[1] = t->rows; info[2] = t->cursor_x; info[3] = t->cursor_y;
    info[4] = t->exited && t->drained; info[5] = t->exit_code;
}
const char *thc_terminal_error(thc_terminal *t) { return t->error; }
#ifndef _WIN32
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
        /* On macOS a dying PTY child can wait for the master's close. Drain
         * its available tail first, then close before waiting for that exit. */
        t->output_length = 0;
        if (t->master >= 0 && fcntl(t->master, F_SETFL, O_NONBLOCK) == 0)
            (void)read_output(t);
        t->preserve_output = 1;
        if (t->master >= 0) { close(t->master); t->master = -1; }
        t->input_length = 0;
        int status;
        pid_t pid;
        do { pid = waitpid(t->pid, &status, 0); } while (pid < 0 && errno == EINTR);
        t->exited = 1;
        t->reaped = 1;
        t->exit_code = pid > 0 ? (WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status)) : 137;
    }
}
#endif
void thc_terminal_free(thc_terminal *t) {
    if (!t) return;
#ifdef _WIN32
    free_process(t);
#else
    thc_terminal_kill(t);
    if (t->master >= 0) close(t->master);
#endif
    if (t->mouse_event) ghostty_mouse_event_free(t->mouse_event);
    if (t->mouse_encoder) ghostty_mouse_encoder_free(t->mouse_encoder);
    if (t->cell) ghostty_render_state_row_cells_free(t->cell);
    if (t->row) ghostty_render_state_row_iterator_free(t->row);
    if (t->render) ghostty_render_state_free(t->render);
    if (t->terminal) ghostty_terminal_free(t->terminal);
    free(t->cells); free(t->text); free(t->input); free(t);
}
