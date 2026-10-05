// SPDX-License-Identifier: BSD-3-Clause
/* Checked no-replace filesystem rename. No fallback may overwrite a target. */
#define _GNU_SOURCE
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
static wchar_t *wide_path(char const *path) {
  int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, NULL, 0);
  if (!n) { errno = EINVAL; return NULL; }
  wchar_t *out = malloc((size_t)n * sizeof *out);
  if (!out) { errno = ENOMEM; return NULL; }
  if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, out, n)) {
    free(out); errno = EINVAL; return NULL;
  }
  return out;
}
static int windows_error(void) {
  DWORD error = GetLastError();
  errno = error == ERROR_FILE_EXISTS || error == ERROR_ALREADY_EXISTS ? EEXIST :
    error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND ? ENOENT : EACCES;
  return -1;
}
int thc_file_stamp(char const *path, uint64_t out[8]) {
  wchar_t *wide = wide_path(path);
  if (!wide) return -1;
  HANDLE h = CreateFileW(wide, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
    NULL, OPEN_EXISTING, FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
  free(wide);
  if (h == INVALID_HANDLE_VALUE) return windows_error();
  BY_HANDLE_FILE_INFORMATION info;
  BOOL ok = GetFileInformationByHandle(h, &info);
  DWORD error = GetLastError();
  CloseHandle(h);
  if (!ok) { SetLastError(error); return windows_error(); }
  out[0] = info.dwVolumeSerialNumber;
  out[1] = ((uint64_t)info.nFileIndexHigh << 32) | info.nFileIndexLow;
  out[2] = info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT ? 4 :
    info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ? 2 : 1;
  out[3] = ((uint64_t)info.nFileSizeHigh << 32) | info.nFileSizeLow;
  out[4] = info.ftLastWriteTime.dwHighDateTime; out[5] = info.ftLastWriteTime.dwLowDateTime;
  out[6] = info.ftCreationTime.dwHighDateTime; out[7] = info.ftCreationTime.dwLowDateTime;
  return 0;
}
#else
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#if defined(__linux__)
#include <linux/fs.h>
#endif
int thc_file_stamp(char const *path, uint64_t out[8]) {
  struct stat st;
  if (lstat(path, &st)) return -1;
  out[0] = st.st_dev; out[1] = st.st_ino;
  out[2] = S_ISREG(st.st_mode) ? 1 : S_ISDIR(st.st_mode) ? 2 : S_ISLNK(st.st_mode) ? 4 : 0;
  out[3] = st.st_size;
#ifdef __APPLE__
  out[4] = st.st_mtimespec.tv_sec; out[5] = st.st_mtimespec.tv_nsec;
  out[6] = st.st_ctimespec.tv_sec; out[7] = st.st_ctimespec.tv_nsec;
#else
  out[4] = st.st_mtim.tv_sec; out[5] = st.st_mtim.tv_nsec;
  out[6] = st.st_ctim.tv_sec; out[7] = st.st_ctim.tv_nsec;
#endif
  return 0;
}
#endif
int thc_rename_noreplace(char const *source, char const *destination, uint64_t const expected[8]) {
  uint64_t current[8];
  if (thc_file_stamp(source, current)) return -1;
  if (memcmp(current, expected, sizeof current)) { errno = EAGAIN; return -1; }
#if defined(__APPLE__)
  return renamex_np(source, destination, RENAME_EXCL);
#elif defined(__linux__)
  return renameat2(AT_FDCWD, source, AT_FDCWD, destination, RENAME_NOREPLACE);
#elif defined(_WIN32)
  wchar_t *old = wide_path(source), *next = wide_path(destination);
  if (!old || !next) { free(old); free(next); return -1; }
  BOOL ok = MoveFileW(old, next);
  DWORD error = GetLastError();
  free(old); free(next);
  if (!ok) { SetLastError(error); return windows_error(); }
  return 0;
#else
  errno = ENOSYS;
  return -1;
#endif
}
