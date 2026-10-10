/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef HIDE_SYSTEM_ONE_TOKENIZER_H
#define HIDE_SYSTEM_ONE_TOKENIZER_H
#include <stddef.h>
#include <stdint.h>

typedef struct HideTokenizer HideTokenizer;
/* JSON and UTF-8 buffers are borrowed only for each call. No Rust exception
 * crosses the ABI. Status: 0 success, 1 failure, 2 invalid input, 3 token limit.
 * A successful creation owns an independent tokenizer until free. */
int hide_tokenizer_create(const uint8_t *json, size_t length, HideTokenizer **out);
int hide_tokenizer_encode(const HideTokenizer *, const uint8_t *utf8, size_t length,
                          uint32_t *tokens, size_t capacity, size_t *written);
void hide_tokenizer_free(HideTokenizer *);
#endif
