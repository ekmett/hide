// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-3-Clause
#include "../cbits/file-drag-windows.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

/* Real Shell consumer, no window or desktop input. The native check runner owns
 * the supplied output directory and removes it even after a failed assertion. */
int main(int argc,char **argv) {
    assert(argc==2);
    char path[32768];
    int length=snprintf(path,sizeof(path),"%s\\export \xce\xbb.bin",argv[1]);
    assert(length>0 && (size_t)length<sizeof(path));
    WCHAR wide[32768];
    assert(MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,path,-1,wide,32768));
    const unsigned char bytes[]={0,255,128,13,10,1,0};
    HANDLE file=CreateFileW(wide,GENERIC_WRITE,0,NULL,CREATE_NEW,FILE_ATTRIBUTE_NORMAL,NULL);
    assert(file!=INVALID_HANDLE_VALUE);
    DWORD written=0;
    assert(WriteFile(file,bytes,sizeof(bytes),&written,NULL) && written==sizeof(bytes));
    assert(CloseHandle(file));

    assert(SUCCEEDED(OleInitialize(NULL)));
    IDataObject *data=NULL;
    assert(SUCCEEDED(hide_file_drag_data(path,&data)) && data);
    FORMATETC format={CF_HDROP,NULL,DVASPECT_CONTENT,-1,TYMED_HGLOBAL};
    assert(SUCCEEDED(IDataObject_QueryGetData(data,&format)));
    STGMEDIUM medium={0};
    assert(SUCCEEDED(IDataObject_GetData(data,&format,&medium)));
    IDataObject_Release(data); /* Receiver's medium owns its storage independently. */
    assert(medium.tymed==TYMED_HGLOBAL);
    assert(DragQueryFileW(medium.hGlobal,0xffffffff,NULL,0)==1);
    WCHAR received[32768];
    assert(DragQueryFileW(medium.hGlobal,0,received,32768)>0);
    assert(wcscmp(received,wide)==0);
    file=CreateFileW(received,GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
    assert(file!=INVALID_HANDLE_VALUE);
    unsigned char copy[sizeof(bytes)+1]; DWORD read=0;
    assert(ReadFile(file,copy,sizeof(copy),&read,NULL) && read==sizeof(bytes));
    assert(memcmp(copy,bytes,sizeof(bytes))==0);
    assert(CloseHandle(file));
    ReleaseStgMedium(&medium);
    assert(GetFileAttributesW(wide)!=INVALID_FILE_ATTRIBUTES); /* Never moves/deletes. */

    assert(FAILED(hide_file_drag_data("\xff",&data)) && !data);
    assert(FAILED(hide_file_drag_data(argv[1],&data)) && !data); /* No directories. */
    assert(DeleteFileW(wide));
    assert(FAILED(hide_file_drag_data(path,&data)) && !data); /* Retired snapshot. */
    OleUninitialize();
    puts("Windows Shell file export checks passed");
    return 0;
}
