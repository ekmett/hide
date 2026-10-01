#ifndef THC_REMOTE_H
#define THC_REMOTE_H
#include <stdint.h>
#include <wchar.h>
uint32_t thc_remote_private_directory(const wchar_t *path);
uint32_t thc_remote_descriptor_write(const wchar_t *path, const uint8_t *bytes, uint32_t count);
uint32_t thc_remote_descriptor_read(const wchar_t *path, uint8_t *bytes, uint32_t capacity, uint32_t *count);
uint32_t thc_remote_random(uint8_t *bytes, uint32_t count);
uint32_t thc_remote_hmac(const uint8_t *key48, const uint8_t *message, uint32_t count, uint8_t *output32);
uint32_t thc_remote_spawn(const wchar_t *application, wchar_t *command, const wchar_t *log_path, void **process);
#endif
