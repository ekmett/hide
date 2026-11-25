/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#ifndef HIDE_ACCESSIBILITY_ACCESSKIT_H
#define HIDE_ACCESSIBILITY_ACCESSKIT_H
#include "accessibility-menu.h"
#include <accesskit.h>
/* Pure owned tree conversion; caller supplies a validated immutable snapshot
 * and holds its publication lock. NULL withdraws all menu children. */
struct accesskit_tree_update *thc_ax_menu_accesskit_tree(const thc_ax_menu_snapshot *snapshot);
#endif
