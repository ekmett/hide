#include "terminal.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

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
int main(void) {
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
    return 0;
}
