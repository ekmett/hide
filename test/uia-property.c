#define COBJMACROS
#define INITGUID
#include <windows.h>
#include <uiautomation.h>
#include <uiautomationclient.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
int main(int argc,char **argv) {
    if(argc!=3) return 2;
    HRESULT initialized=CoInitializeEx(NULL,COINIT_MULTITHREADED),hr;
    IUIAutomation *automation=NULL;IUIAutomationElement *root=NULL,*node=NULL;IUIAutomationCondition *condition=NULL;
    VARIANT name,value;VariantInit(&name);VariantInit(&value);
    wchar_t wide[256];if(!MultiByteToWideChar(CP_UTF8,0,argv[2],-1,wide,256)) return 3;
    hr=CoCreateInstance(&CLSID_CUIAutomation,NULL,CLSCTX_INPROC_SERVER,&IID_IUIAutomation,(void**)&automation);
    if(FAILED(hr)) goto fail;
    hr=IUIAutomation_ElementFromHandle(automation,(HWND)(uintptr_t)strtoull(argv[1],NULL,10),&root);if(FAILED(hr)) goto fail;
    V_VT(&name)=VT_BSTR;V_BSTR(&name)=SysAllocString(wide);
    hr=IUIAutomation_CreatePropertyCondition(automation,UIA_NamePropertyId,name,&condition);if(FAILED(hr)) goto fail;
    hr=IUIAutomationElement_FindFirst(root,TreeScope_Subtree,condition,&node);if(FAILED(hr)||!node) goto fail;
    hr=IUIAutomationElement_GetCurrentPropertyValue(node,UIA_FullDescriptionPropertyId,&value);if(FAILED(hr)||V_VT(&value)!=VT_BSTR) goto fail;
    char utf8[2048];int count=WideCharToMultiByte(CP_UTF8,0,V_BSTR(&value),-1,utf8,sizeof(utf8),NULL,NULL);if(!count) goto fail;
    fwrite(utf8,1,count-1,stdout);fputc('\n',stdout);
    VariantClear(&name);VariantClear(&value);IUIAutomationElement_Release(node);IUIAutomationCondition_Release(condition);IUIAutomationElement_Release(root);IUIAutomation_Release(automation);if(SUCCEEDED(initialized))CoUninitialize();return 0;
fail:
    fprintf(stderr,"UIA inspect HRESULT=%08lx\n",(unsigned long)hr);return 1;
}
