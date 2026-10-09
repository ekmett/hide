// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-3-Clause
#ifndef HIDE_FILE_DRAG_WINDOWS_H
#define HIDE_FILE_DRAG_WINDOWS_H
#define COBJMACROS
#include <windows.h>
#include <shlobj.h>

/* Build the Shell transfer object for one existing staged file (UTF-8 absolute
 * path). Caller owns the returned COM reference and must initialize OLE on this
 * thread. FileExports owns the file; releasing this object never removes it. */
HRESULT hide_file_drag_data(const char *path, IDataObject **data);
#endif
