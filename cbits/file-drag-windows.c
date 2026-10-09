// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-3-Clause
#include "file-drag-windows.h"
#include <stdlib.h>

HRESULT hide_file_drag_data(const char *path, IDataObject **data) {
    if (!data) return E_POINTER;
    *data=NULL;
    if (!path || !*path) return E_INVALIDARG;
    int length=MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,path,-1,NULL,0);
    if (!length) return HRESULT_FROM_WIN32(GetLastError());
    WCHAR *wide=malloc((size_t)length*sizeof(*wide));
    if (!wide) return E_OUTOFMEMORY;
    if (!MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,path,-1,wide,length)) {
        HRESULT result=HRESULT_FROM_WIN32(GetLastError()); free(wide); return result;
    }
    DWORD attributes=GetFileAttributesW(wide);
    HRESULT result;
    if (attributes==INVALID_FILE_ATTRIBUTES) result=HRESULT_FROM_WIN32(GetLastError());
    else if (attributes&FILE_ATTRIBUTE_DIRECTORY) result=E_INVALIDARG;
    else {
        IShellItem *item=NULL;
        result=SHCreateItemFromParsingName(wide,NULL,&IID_IShellItem,(void **)&item);
        if (SUCCEEDED(result)) {
            result=IShellItem_BindToHandler(item,NULL,&BHID_DataObject,&IID_IDataObject,(void **)data);
            IShellItem_Release(item);
        }
    }
    free(wide);
    return result;
}
