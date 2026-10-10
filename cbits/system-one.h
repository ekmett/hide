/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef HIDE_SYSTEM_ONE_H
#define HIDE_SYSTEM_ONE_H

#include <stddef.h>
#include <stdint.h>

typedef struct HideSystemOne HideSystemOne;

/* The owner serializes load/begin/run/free. cancel may run concurrently with
 * load/run, but must join before free. Status: 0 success, 1 failure, 2 cancelled,
 * 3 invalid tensor dimensions, 4 tracked allocation limit reached. */
int hide_system_one_create(uint64_t limit, unsigned threads, HideSystemOne **out);
int hide_system_one_load(HideSystemOne *, const char *encoder, const char *head);
int hide_system_one_begin(HideSystemOne *);
void hide_system_one_cancel(HideSystemOne *);
int hide_system_one_run(HideSystemOne *, const int64_t *ids, size_t length,
                        const int64_t *markers, size_t count, int64_t qtype,
                        float *logits);
uint64_t hide_system_one_used(const HideSystemOne *);
uint64_t hide_system_one_peak(const HideSystemOne *);
/* Returns remaining tracked bytes after session release; normally zero. */
uint64_t hide_system_one_free(HideSystemOne *);

#endif
