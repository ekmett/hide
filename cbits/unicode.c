#include "unicode.h"
#include <utf8proc.h>

uint64_t thc_grapheme_step(int previous, int current, int state) {
    utf8proc_int32_t next=state;
    int boundary=utf8proc_grapheme_break_stateful(previous,current,&next);
    return ((uint64_t)(uint32_t)next<<1) | (boundary!=0);
}
