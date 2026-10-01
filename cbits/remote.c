/* Windows session descriptors inherit no access from other local accounts. */
#define WIN32_LEAN_AND_MEAN
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#include <windows.h>
#include <aclapi.h>
#include <wincrypt.h>
#include <stdlib.h>
#include <string.h>
#include "remote.h"

typedef struct {
    TOKEN_USER *user;
    ACL *acl;
    SECURITY_DESCRIPTOR descriptor;
    SECURITY_ATTRIBUTES attributes;
} PrivateSecurity;

static void free_security(PrivateSecurity *security) {
    free(security->acl);
    free(security->user);
}
static DWORD private_security(PrivateSecurity *security) {
    HANDLE token;
    DWORD size = 0, error = ERROR_SUCCESS;
    memset(security, 0, sizeof(*security));
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) return GetLastError();
    GetTokenInformation(token, TokenUser, NULL, 0, &size);
    security->user = malloc(size);
    if (!security->user) { CloseHandle(token); return ERROR_NOT_ENOUGH_MEMORY; }
    if (!GetTokenInformation(token, TokenUser, security->user, size, &size)) error = GetLastError();
    CloseHandle(token);
    if (error) return error;
    size = sizeof(ACL) + sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD) + GetLengthSid(security->user->User.Sid);
    security->acl = malloc(size);
    if (!security->acl) return ERROR_NOT_ENOUGH_MEMORY;
    if (!InitializeAcl(security->acl, size, ACL_REVISION) ||
        !AddAccessAllowedAceEx(security->acl, ACL_REVISION, OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE,
                              FILE_ALL_ACCESS, security->user->User.Sid) ||
        !InitializeSecurityDescriptor(&security->descriptor, SECURITY_DESCRIPTOR_REVISION) ||
        !SetSecurityDescriptorOwner(&security->descriptor, security->user->User.Sid, FALSE) ||
        !SetSecurityDescriptorDacl(&security->descriptor, TRUE, security->acl, FALSE) ||
        !SetSecurityDescriptorControl(&security->descriptor, SE_DACL_PROTECTED, SE_DACL_PROTECTED)) return GetLastError();
    security->attributes.nLength = sizeof(SECURITY_ATTRIBUTES);
    security->attributes.lpSecurityDescriptor = &security->descriptor;
    security->attributes.bInheritHandle = FALSE;
    return ERROR_SUCCESS;
}

static DWORD check_private(HANDLE handle, PSID user, int directory) {
    BY_HANDLE_FILE_INFORMATION info;
    PSECURITY_DESCRIPTOR descriptor = NULL;
    PACL acl = NULL;
    PSID owner = NULL;
    DWORD error, revision;
    SECURITY_DESCRIPTOR_CONTROL control;
    if (!GetFileInformationByHandle(handle, &info)) return GetLastError();
    if ((info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) ||
        (!!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != !!directory)) return ERROR_ACCESS_DENIED;
    error = GetSecurityInfo(handle, SE_FILE_OBJECT, OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
                            &owner, NULL, &acl, NULL, &descriptor);
    if (error) return error;
    if (!owner || !EqualSid(owner, user) || !acl || !acl->AceCount) error = ERROR_ACCESS_DENIED;
    if (!error && directory && (!GetSecurityDescriptorControl(descriptor, &control, &revision) || !(control & SE_DACL_PROTECTED))) error = ERROR_ACCESS_DENIED;
    for (DWORD i = 0; !error && i < acl->AceCount; ++i) {
        ACCESS_ALLOWED_ACE *ace;
        if (!GetAce(acl, i, (void **)&ace) || ace->Header.AceType != ACCESS_ALLOWED_ACE_TYPE ||
            !EqualSid(&ace->SidStart, user)) error = ERROR_ACCESS_DENIED;
    }
    LocalFree(descriptor);
    return error;
}

uint32_t thc_remote_private_directory(const wchar_t *path) {
    PrivateSecurity security;
    DWORD error = private_security(&security);
    if (!error && !CreateDirectoryW(path, &security.attributes) && GetLastError() != ERROR_ALREADY_EXISTS) error = GetLastError();
    if (!error) {
        HANDLE handle = CreateFileW(path, READ_CONTROL, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                                    NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
        if (handle == INVALID_HANDLE_VALUE) error = GetLastError();
        else { error = check_private(handle, security.user->User.Sid, 1); CloseHandle(handle); }
    }
    free_security(&security);
    return error;
}

uint32_t thc_remote_descriptor_write(const wchar_t *path, const uint8_t *bytes, uint32_t count) {
    PrivateSecurity security;
    DWORD error = private_security(&security), written;
    if (!error) {
        HANDLE handle = CreateFileW(path, GENERIC_WRITE, FILE_SHARE_READ, &security.attributes,
                                    CREATE_NEW, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
        if (handle == INVALID_HANDLE_VALUE) error = GetLastError();
        else {
            if (!WriteFile(handle, bytes, count, &written, NULL)) error = GetLastError();
            else if (written != count) error = ERROR_WRITE_FAULT;
            else if (!FlushFileBuffers(handle)) error = GetLastError();
            CloseHandle(handle);
            if (error) DeleteFileW(path);
        }
    }
    free_security(&security);
    return error;
}

uint32_t thc_remote_descriptor_read(const wchar_t *path, uint8_t *bytes, uint32_t capacity, uint32_t *count) {
    PrivateSecurity security;
    DWORD error = private_security(&security), read;
    *count = 0;
    if (!error) {
        HANDLE handle = CreateFileW(path, GENERIC_READ | READ_CONTROL, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                                    NULL, OPEN_EXISTING, FILE_FLAG_OPEN_REPARSE_POINT, NULL);
        if (handle == INVALID_HANDLE_VALUE) error = GetLastError();
        else {
            LARGE_INTEGER size;
            error = check_private(handle, security.user->User.Sid, 0);
            if (!error && !GetFileSizeEx(handle, &size)) error = GetLastError();
            if (!error && (size.QuadPart < 0 || size.QuadPart > capacity)) error = ERROR_INVALID_DATA;
            if (!error) {
                if (!ReadFile(handle, bytes, capacity, &read, NULL)) error = GetLastError();
                else *count = read;
            }
            CloseHandle(handle);
        }
    }
    free_security(&security);
    return error;
}

uint32_t thc_remote_random(uint8_t *bytes, uint32_t count) {
    HCRYPTPROV provider;
    DWORD error = ERROR_SUCCESS;
    if (!CryptAcquireContextW(&provider, NULL, NULL, PROV_RSA_AES, CRYPT_VERIFYCONTEXT | CRYPT_SILENT)) return GetLastError();
    if (!CryptGenRandom(provider, count, bytes)) error = GetLastError();
    CryptReleaseContext(provider, 0);
    return error;
}

static DWORD sha256(HCRYPTPROV provider, const uint8_t *prefix, DWORD prefix_size,
                    const uint8_t *bytes, DWORD count, uint8_t *output) {
    HCRYPTHASH hash;
    DWORD error = ERROR_SUCCESS, size = 32;
    if (!CryptCreateHash(provider, CALG_SHA_256, 0, 0, &hash)) return GetLastError();
    if (!CryptHashData(hash, prefix, prefix_size, 0) || !CryptHashData(hash, bytes, count, 0) ||
        !CryptGetHashParam(hash, HP_HASHVAL, output, &size, 0)) error = GetLastError();
    CryptDestroyHash(hash);
    return error;
}
uint32_t thc_remote_hmac(const uint8_t *key48, const uint8_t *message, uint32_t count, uint8_t *output32) {
    HCRYPTPROV provider;
    uint8_t inner_key[64], outer_key[64], digest[32];
    DWORD error;
    if (!CryptAcquireContextW(&provider, NULL, NULL, PROV_RSA_AES, CRYPT_VERIFYCONTEXT | CRYPT_SILENT)) return GetLastError();
    for (int i = 0; i < 64; ++i) {
        uint8_t key = i < 48 ? key48[i] : 0;
        inner_key[i] = key ^ 0x36; outer_key[i] = key ^ 0x5c;
    }
    error = sha256(provider, inner_key, 64, message, count, digest);
    if (!error) error = sha256(provider, outer_key, 64, digest, 32, output32);
    SecureZeroMemory(inner_key, sizeof(inner_key)); SecureZeroMemory(outer_key, sizeof(outer_key)); SecureZeroMemory(digest, sizeof(digest));
    CryptReleaseContext(provider, 0);
    return error;
}
