#include "terminal.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <wchar.h>
#include <shellapi.h>
static char observed[4096];
static size_t poll_until(thc_terminal *, const char *, int);
static thc_terminal *spawn_mode(const wchar_t *, int, int);
static volatile LONG interrupted;
static BOOL WINAPI interrupted_console(DWORD event) {
    if (event != CTRL_C_EVENT) return FALSE;
    InterlockedExchange(&interrupted, 1);
    return TRUE;
}
#endif

static void feed(thc_terminal *t, const char *text) {
    thc_terminal_feed(t, (const uint8_t *)text, strlen(text));
    if (!thc_terminal_poll(t)) { fprintf(stderr, "%s\n", thc_terminal_error(t)); assert(0); }
}
static void text_at(thc_terminal *t, int index, const char *expected) {
    const uint32_t *cell = thc_terminal_cells(t) + index * 6;
    size_t length;
    const uint8_t *text = thc_terminal_text(t, &length);
    assert(cell[0] + cell[1] <= length);
    assert(cell[1] == strlen(expected));
    assert(!cell[1] || !memcmp(text + cell[0], expected, cell[1]));
}
#ifdef _WIN32
/* Child modes exercise actual console APIs, independent of cmd/PowerShell. */
static int child_mode(const char *mode) {
    if (!strcmp(mode, "--no-console")) {
        FreeConsole();
        thc_terminal *terminal = spawn_mode(L"--ctrl-c", 80, 25);
        poll_until(terminal, "CTRL-READY", 0);
        assert(thc_terminal_write(terminal, (const uint8_t *)"\003", 1));
        poll_until(terminal, "CTRL-RECEIVED", 0);
        poll_until(terminal, NULL, 0);
        thc_terminal_free(terminal);
        return 0;
    }
    if (!strcmp(mode, "--parent-control")) {
        /* Use a fixture-owned console so generated events cannot hit SSH or
         * another user's editor. Installed handlers must survive terminal start. */
        FreeConsole(); assert(AllocConsole());
        assert(SetConsoleCtrlHandler(NULL, FALSE));
        assert(SetConsoleCtrlHandler(interrupted_console, TRUE));
        assert(GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0));
        for (int i = 0; i < 100 && !interrupted; ++i) Sleep(10);
        assert(interrupted);
        InterlockedExchange(&interrupted, 0);
        thc_terminal *terminal = spawn_mode(L"--hold", 80, 25);
        assert(GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0));
        for (int i = 0; i < 100 && !interrupted; ++i) Sleep(10);
        assert(interrupted);
        thc_terminal_free(terminal);
        return 0;
    }
    if (!strcmp(mode, "--arguments")) {
        int count; wchar_t **args = CommandLineToArgvW(GetCommandLineW(), &count);
        assert(args && count == 8);
        const wchar_t *expected[] = {L"", L"two words", L"a\"b", L"trailing\\", L"space trailing\\", L"\u00e9\u754c"};
        for (int i = 0; i < 6; ++i) assert(!wcscmp(args[i+2], expected[i]));
        LocalFree(args);
        wchar_t cwd[32768], expected_cwd[32768], value[100];
        assert(GetCurrentDirectoryW(32768, cwd));
        assert(GetEnvironmentVariableW(L"THC_EXPECTED_CWD", expected_cwd, 32768));
        assert(!wcscmp(cwd, expected_cwd));
        assert(GetEnvironmentVariableW(L"THC_TERMINAL_TEST", value, 100));
        assert(!wcscmp(value, L"value \u00e9\u754c"));
        puts("ARGV-CWD-ENV-OK"); fflush(stdout);
        return 0;
    }
    if (!strcmp(mode, "--bulk")) {
        DWORD mode, n; HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
        HANDLE output = GetStdHandle(STD_OUTPUT_HANDLE);
        assert(SetConsoleMode(input, 0));
        assert(GetConsoleMode(output, &mode));
        assert(SetConsoleMode(output, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING));
        unsigned random = 1;
        for (int frame = 0; frame < 20; ++frame) {
            char cells[200*99];
            for (size_t i = 0; i < sizeof(cells); ++i) {
                random = random * 1664525u + 1013904223u;
                cells[i] = (char)('A' + ((random >> 16) % 26));
            }
            assert(WriteFile(output, "\033[2J\033[H", 7, &n, NULL));
            assert(WriteFile(output, cells, sizeof(cells), &n, NULL));
            char marker[12];
            for (int i = 0; i < 12; ++i) marker[i] = (char)('a' + (frame + i) % 26);
            assert(WriteFile(output, "\033[100;1H", 8, &n, NULL));
            assert(WriteFile(output, marker, sizeof(marker), &n, NULL));
            /* Acknowledge each visible frame, preventing ConPTY from replacing
             * intermediate screen states before its renderer flushes them. */
            char ack; assert(ReadFile(input, &ack, 1, &n, NULL) && n == 1);
        }
        assert(WriteFile(output, "FINAL-DRAIN", 11, &n, NULL));
        return 0;
    }
    if (!strcmp(mode, "--flood")) {
        char block[200*99]; unsigned random = 1; DWORD n, flags;
        HANDLE output = GetStdHandle(STD_OUTPUT_HANDLE);
        assert(GetConsoleMode(output, &flags));
        assert(SetConsoleMode(output, flags | ENABLE_VIRTUAL_TERMINAL_PROCESSING));
        for (;;) {
            for (size_t i = 0; i < sizeof(block); ++i) {
                random = random * 1664525u + 1013904223u;
                block[i] = (char)('A' + ((random >> 16) % 26));
            }
            if (!WriteFile(output, "\033[H", 3, &n, NULL) ||
                !WriteFile(output, block, sizeof(block), &n, NULL)) return 1;
            Sleep(5);
        }
    }
    if (!strcmp(mode, "--ctrl-c")) {
        DWORD flags; HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
        assert(GetConsoleMode(input, &flags));
        assert(SetConsoleMode(input, flags | ENABLE_PROCESSED_INPUT));
        assert(SetConsoleCtrlHandler(interrupted_console, TRUE));
        puts("CTRL-READY"); fflush(stdout);
        while (!InterlockedCompareExchange(&interrupted, 0, 0)) Sleep(5);
        puts("CTRL-RECEIVED"); fflush(stdout);
        return 0;
    }
    if (!strcmp(mode, "--tree")) {
        wchar_t exe[32768], command[32768];
        assert(GetModuleFileNameW(NULL, exe, 32768));
        assert(swprintf(command, 32768, L"\"%ls\" --hold", exe) > 0);
        STARTUPINFOW startup = {.cb = sizeof(startup)};
        PROCESS_INFORMATION child = {0};
        assert(CreateProcessW(exe, command, NULL, NULL, FALSE, 0, NULL, NULL, &startup, &child));
        printf("PROCESS-PIDS=%lu,%lu TREE-READY\n", (unsigned long)GetCurrentProcessId(), (unsigned long)child.dwProcessId);
        fflush(stdout);
        CloseHandle(child.hThread); CloseHandle(child.hProcess);
        Sleep(30000);
        return 0;
    }
    if (!strcmp(mode, "--owner")) {
        /* The external native runner kills this owner after reading its PIDs,
         * then verifies both processes die through JOB_OBJECT_KILL_ON_JOB_CLOSE. */
        thc_terminal *terminal = spawn_mode(L"--tree", 120, 25);
        poll_until(terminal, "TREE-READY", 0);
        char *pids = strstr(observed, "PROCESS-PIDS="); assert(pids);
        unsigned long root, descendant;
        assert(sscanf(pids, "PROCESS-PIDS=%lu,%lu", &root, &descendant) == 2);
        printf("OWNED-PIDS=%lu,%lu\n", root, descendant); fflush(stdout);
        Sleep(30000);
        thc_terminal_free(terminal);
        return 0;
    }
    if (!strcmp(mode, "--hold")) { Sleep(30000); return 0; }
    if (!strcmp(mode, "--interactive")) {
        DWORD n; wchar_t input[100];
        assert(WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE), L"READY \u00e9\u754c\r\n", 10, &n, NULL));
        assert(ReadConsoleW(GetStdHandle(STD_INPUT_HANDLE), input, 99, &n, NULL));
        input[n] = 0;
        assert(wcsstr(input, L"hello \u00e9\u754c"));
        CONSOLE_SCREEN_BUFFER_INFO info;
        assert(GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info));
        assert(info.dwSize.X == 31 && info.dwSize.Y == 9);
        wchar_t value[100]; assert(GetEnvironmentVariableW(L"THC_TERMINAL_TEST", value, 100));
        assert(!wcscmp(value, L"value \u00e9"));
        printf("INPUT-AND-SIZE-OK\n"); fflush(stdout);
        return 7;
    }
    return -1;
}
static thc_terminal *spawn_mode(const wchar_t *mode, int columns, int rows) {
    wchar_t exe[32768], command[32768], cwd[32768];
    assert(GetModuleFileNameW(NULL, exe, 32768));
    assert(GetCurrentDirectoryW(32768, cwd));
    assert(swprintf(command, 32768, L"\"%ls\" %ls", exe, mode) > 0);
    const wchar_t variable[] = L"THC_TERMINAL_TEST=value \u00e9\0";
    wchar_t *inherited = GetEnvironmentStringsW(); assert(inherited);
    size_t length = 0;
    while (inherited[length]) length += wcslen(inherited + length) + 1;
    wchar_t *environment = malloc(length * sizeof(wchar_t) + sizeof(variable)); assert(environment);
    memcpy(environment, inherited, length * sizeof(wchar_t));
    memcpy(environment + length, variable, sizeof(variable));
    FreeEnvironmentStringsW(inherited);
    fwprintf(stderr, L"ConPTY fixture %ls\n", mode);
    thc_terminal *t = thc_terminal_new(columns, rows); assert(t);
    if (!thc_terminal_spawn_windows(t, exe, command, environment, cwd)) {
        fprintf(stderr, "%s\n", thc_terminal_error(t)); assert(0);
    }
    free(environment);
    return t;
}
static size_t poll_until(thc_terminal *t, const char *marker, int expected_exit) {
    ULONGLONG deadline = GetTickCount64() + 10000;
    observed[0] = 0; size_t kept = 0, total = 0;
    for (;;) {
        if (GetTickCount64() >= deadline) {
            fprintf(stderr, "Timed out waiting for %s; observed: %s\n", marker ? marker : "process exit", observed);
            assert(0);
        }
        if (!thc_terminal_poll(t)) { fprintf(stderr, "%s\n", thc_terminal_error(t)); assert(0); }
        size_t n; const uint8_t *bytes = thc_terminal_output(t, &n);
        total += n;
        size_t copy = n < sizeof(observed)-1 ? n : sizeof(observed)-1;
        if (kept + copy >= sizeof(observed)) {
            size_t drop = kept + copy - sizeof(observed) + 1;
            memmove(observed, observed + drop, kept - drop); kept -= drop;
        }
        memcpy(observed+kept, bytes+n-copy, copy); kept += copy; observed[kept] = 0;
        if (marker && strstr(observed, marker)) return total;
        int info[6]; thc_terminal_info(t, info);
        if (info[4]) { assert(!marker); assert(info[5] == expected_exit); return total; }
        Sleep(5);
    }
}
static void process_checks(void) {
    thc_terminal *t = spawn_mode(L"--interactive", 20, 5);
    poll_until(t, "READY \xc3\xa9\xe7\x95\x8c", 0);
    assert(thc_terminal_resize(t, 31, 9));
    const char *line = "hello \xc3\xa9\xe7\x95\x8c\r";
    assert(thc_terminal_write(t, (const uint8_t *)line, strlen(line)));
    poll_until(t, NULL, 7); thc_terminal_free(t);
    t = spawn_mode(L"--ctrl-c", 80, 25);
    poll_until(t, "CTRL-READY", 0);
    assert(thc_terminal_write(t, (const uint8_t *)"\003", 1));
    poll_until(t, "CTRL-RECEIVED", 0);
    poll_until(t, NULL, 0); thc_terminal_free(t);
    t = spawn_mode(L"--tree", 120, 25);
    poll_until(t, "TREE-READY", 0);
    char *pids = strstr(observed, "PROCESS-PIDS="); assert(pids);
    unsigned long root, descendant;
    assert(sscanf(pids, "PROCESS-PIDS=%lu,%lu", &root, &descendant) == 2);
    HANDLE processes[2] = {OpenProcess(SYNCHRONIZE, FALSE, root), OpenProcess(SYNCHRONIZE, FALSE, descendant)};
    assert(processes[0] && processes[1]);
    assert(WaitForSingleObject(processes[0], 0) == WAIT_TIMEOUT);
    assert(WaitForSingleObject(processes[1], 0) == WAIT_TIMEOUT);
    thc_terminal_kill(t);
    poll_until(t, NULL, 137); thc_terminal_free(t);
    assert(WaitForMultipleObjects(2, processes, TRUE, 5000) == WAIT_OBJECT_0);
    CloseHandle(processes[0]); CloseHandle(processes[1]);
    t = spawn_mode(L"--bulk", 200, 100);
    size_t total = 0;
    for (int frame = 0; frame < 20; ++frame) {
        char marker[13] = {0};
        for (int i = 0; i < 12; ++i) marker[i] = (char)('a' + (frame + i) % 26);
        total += poll_until(t, marker, 0);
        assert(thc_terminal_write(t, (const uint8_t *)".", 1));
    }
    total += poll_until(t, "FINAL-DRAIN", 0);
    total += poll_until(t, NULL, 0);
    assert(total > 256*1024); thc_terminal_free(t);
    t = spawn_mode(L"--hold", 80, 25);
    uint8_t *input = malloc(1024*1024+1); assert(input); memset(input, 'a', 1024*1024+1);
    assert(!thc_terminal_write(t, input, 1024*1024+1));
    assert(thc_terminal_write(t, input, 1024*1024)); free(input);
    ULONGLONG before = GetTickCount64();
    thc_terminal_kill(t); thc_terminal_kill(t);
    poll_until(t, NULL, 137); thc_terminal_free(t);
    assert(GetTickCount64()-before < 5000);
    t = spawn_mode(L"--flood", 200, 100); Sleep(2000);
    before = GetTickCount64();
    assert(thc_terminal_resize(t, 201, 100));
    assert(GetTickCount64()-before < 5000);
    assert(thc_terminal_poll(t));
    size_t queued; thc_terminal_output(t, &queued);
    assert(queued >= 256*1024-4096); /* Prove this exercised a full reader queue. */
    Sleep(1000);
    before = GetTickCount64(); thc_terminal_free(t);
    assert(GetTickCount64()-before < 5000);
    puts("native ConPTY process checks passed");
}
#endif
int main(int argc, char **argv) {
#ifdef _WIN32
    if (argc >= 2) return child_mode(argv[1]);
#else
    (void)argc; (void)argv;
#endif
    thc_terminal *t = thc_terminal_new(12, 4);
    assert(t);
    feed(t, "\033[38;2;12;34;56m\033[48;2;65;43;21m\033[1mA\033[0m\xc3\xa9\xe7\x95\x8c");
    const uint32_t *cells = thc_terminal_cells(t);
    text_at(t, 0, "A"); text_at(t, 1, "\xc3\xa9"); text_at(t, 2, "\xe7\x95\x8c");
    assert(cells[2] == 0x0c2238 && cells[3] == 0x412b15 && (cells[4] & 1));
    assert(cells[2*6+5] == 2 && cells[3*6+5] == 0);
    assert(thc_terminal_appearance(t, 0));
    feed(t, ""); cells = thc_terminal_cells(t);
    assert(cells[6+2] == 0 && cells[6+3] == 0xaaaaaa);
    assert(cells[2] == 0x0c2238 && cells[3] == 0x412b15);
    assert(thc_terminal_appearance(t, 1));
    feed(t, ""); cells = thc_terminal_cells(t);
    assert(cells[6+2] == 0xffffff && cells[6+3] == 0);
    int info[6]; thc_terminal_info(t, info);
    assert(info[2] == 4 && info[3] == 0);
    feed(t, "\033[2;3HZ\033[?25l");
    text_at(t, 14, "Z"); thc_terminal_info(t, info); assert(info[2] == -1);
    feed(t, "\033[?25h\033[2J\033[He\xcc\x81");
    text_at(t, 0, "e\xcc\x81"); text_at(t, 14, "");
    /* UTF-8 may arrive across separate PTY reads. */
    feed(t, "\xc3"); feed(t, "\xb1"); text_at(t, 1, "\xc3\xb1");
    assert(thc_terminal_resize(t, 20, 6));
    assert(thc_terminal_poll(t)); thc_terminal_info(t, info);
    assert(info[0] == 20 && info[1] == 6);
    text_at(t, 0, "e\xcc\x81");
    assert(!thc_terminal_resize(t, 0, 6));
    thc_terminal_free(t);
    puts("native terminal parser checks passed");
#ifdef _WIN32
    process_checks();
#endif
    return 0;
}
