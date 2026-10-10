/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#include "system-one.h"
#include <onnxruntime_c_api.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#endif

typedef struct {
    OrtAllocator api;
    OrtMemoryInfo *info;
    _Atomic size_t used, peak;
    size_t limit;
    atomic_bool exhausted;
} Budget;

typedef struct { void *base; size_t bytes; } Allocation;

struct HideSystemOne {
    const OrtApi *api;
    OrtEnv *env;
    OrtSessionOptions *options;
    OrtRunOptions *run;
    OrtSession *encoder, *head;
    Budget budget;
    atomic_bool cancelled;
};

static void *ORT_API_CALL allocate(OrtAllocator *raw, size_t size) {
    Budget *budget = (Budget *)raw;
    if (size == 0) return NULL;
    if (size > SIZE_MAX - 63 - sizeof(Allocation)) return NULL;
    size_t bytes = size + 63 + sizeof(Allocation);
    size_t used = atomic_load(&budget->used);
    do {
        if (used > budget->limit || bytes > budget->limit - used) {
            atomic_store(&budget->exhausted, true);
            return NULL;
        }
    } while (!atomic_compare_exchange_weak(&budget->used, &used, used + bytes));
    void *base = malloc(bytes);
    if (!base) { atomic_fetch_sub(&budget->used, bytes); return NULL; }
    uintptr_t address = ((uintptr_t)base + sizeof(Allocation) + 63) & ~(uintptr_t)63;
    Allocation *header = (Allocation *)address - 1;
    header->base = base;
    header->bytes = bytes;
    size_t peak = atomic_load(&budget->peak);
    while (peak < used + bytes &&
           !atomic_compare_exchange_weak(&budget->peak, &peak, used + bytes)) {}
    return (void *)address;
}

static void ORT_API_CALL deallocate(OrtAllocator *raw, void *pointer) {
    if (!pointer) return;
    Budget *budget = (Budget *)raw;
    Allocation header = *((Allocation *)pointer - 1);
    free(header.base);
    atomic_fetch_sub(&budget->used, header.bytes);
}

static const OrtMemoryInfo *ORT_API_CALL memory_info(const OrtAllocator *raw) {
    return ((const Budget *)raw)->info;
}

static void ORT_API_CALL quiet_log(void *context, OrtLoggingLevel level,
        const char *category, const char *id, const char *location, const char *message) {
    (void)context; (void)level; (void)category; (void)id; (void)location; (void)message;
}

static int status(HideSystemOne *owner, OrtStatus *error) {
    if (error) owner->api->ReleaseStatus(error);
    if (atomic_load(&owner->cancelled)) return 2;
    if (atomic_load(&owner->budget.exhausted)) return 4;
    return error ? 1 : 0;
}

#define CHECK(call) do { OrtStatus *error = (call); if (error) return status(owner, error); } while (0)

int hide_system_one_create(uint64_t limit, unsigned threads, HideSystemOne **out) {
    if (!out || limit == 0 || limit > SIZE_MAX || threads == 0 || threads > 8) return 3;
    *out = NULL;
    HideSystemOne *owner = calloc(1, sizeof(*owner));
    if (!owner) return 1;
    owner->api = OrtGetApiBase()->GetApi(ORT_API_VERSION);
    if (!owner->api) { free(owner); return 1; }
    owner->budget.limit = (size_t)limit;
    owner->budget.api.version = ORT_API_VERSION;
    owner->budget.api.Alloc = allocate;
    owner->budget.api.Free = deallocate;
    owner->budget.api.Info = memory_info;
    owner->budget.api.Reserve = allocate;
    atomic_init(&owner->budget.used, 0);
    atomic_init(&owner->budget.peak, 0);
    atomic_init(&owner->budget.exhausted, false);
    atomic_init(&owner->cancelled, false);
    *out = owner; /* Partial acquisition is always releasable by the caller. */
    CHECK(owner->api->CreateCpuMemoryInfo(OrtDeviceAllocator, OrtMemTypeDefault, &owner->budget.info));
    CHECK(owner->api->CreateEnvWithCustomLogger(quiet_log, NULL, ORT_LOGGING_LEVEL_FATAL, "hide", &owner->env));
    CHECK(owner->api->RegisterAllocator(owner->env, &owner->budget.api));
    CHECK(owner->api->CreateSessionOptions(&owner->options));
    CHECK(owner->api->DisableCpuMemArena(owner->options));
    CHECK(owner->api->AddSessionConfigEntry(owner->options, "session.use_env_allocators", "1"));
    CHECK(owner->api->SetIntraOpNumThreads(owner->options, (int)threads));
    CHECK(owner->api->SetInterOpNumThreads(owner->options, 1));
    CHECK(owner->api->SetSessionGraphOptimizationLevel(owner->options, ORT_ENABLE_BASIC));
    CHECK(owner->api->CreateRunOptions(&owner->run));
    return 0;
}

static OrtStatus *load_session(HideSystemOne *owner, const char *path, OrtSession **session) {
#ifdef _WIN32
    int size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, NULL, 0);
    if (!size) return owner->api->CreateStatus(ORT_INVALID_ARGUMENT, "Invalid model path");
    wchar_t *wide = malloc((size_t)size * sizeof(wchar_t));
    if (!wide) return owner->api->CreateStatus(ORT_FAIL, "Model path allocation failed");
    MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, wide, size);
    OrtStatus *error = owner->api->CreateSession(owner->env, wide, owner->options, session);
    free(wide);
    return error;
#else
    return owner->api->CreateSession(owner->env, path, owner->options, session);
#endif
}

int hide_system_one_load(HideSystemOne *owner, const char *encoder, const char *head) {
    if (!owner || !encoder || !head || owner->encoder || owner->head) return 3;
    if (atomic_load(&owner->cancelled)) return 2;
    CHECK(load_session(owner, encoder, &owner->encoder));
    if (atomic_load(&owner->cancelled)) return 2;
    CHECK(load_session(owner, head, &owner->head));
    return status(owner, NULL);
}

int hide_system_one_begin(HideSystemOne *owner) {
    if (!owner || !owner->run || !owner->encoder || !owner->head) return 3;
    atomic_store(&owner->cancelled, false);
    atomic_store(&owner->budget.exhausted, false);
    CHECK(owner->api->RunOptionsUnsetTerminate(owner->run));
    return 0;
}

void hide_system_one_cancel(HideSystemOne *owner) {
    if (!owner) return;
    atomic_store(&owner->cancelled, true);
    OrtStatus *error = owner->api->SessionOptionsSetLoadCancellationFlag(owner->options, true);
    if (error) owner->api->ReleaseStatus(error);
    error = owner->api->RunOptionsSetTerminate(owner->run);
    if (error) owner->api->ReleaseStatus(error);
}

int hide_system_one_run(HideSystemOne *owner, const int64_t *ids, size_t length,
        const int64_t *markers, size_t count, int64_t qtype, float *logits) {
    if (!owner || !owner->encoder || !owner->head || !ids || !markers || !logits ||
        length < 1 || length > 512 || count < 1 || count > 255 || qtype < 0 || qtype > 2) return 3;
    if (atomic_load(&owner->cancelled)) return 2;
    int64_t attention[512], positions[255];
    bool mask[255];
    size_t width = count < 2 ? 2 : count;
    for (size_t i = 0; i < length; ++i) attention[i] = 1;
    for (size_t i = 0; i < count; ++i) {
        if (markers[i] < 0 || (uint64_t)markers[i] >= length) return 3;
        positions[i] = markers[i]; mask[i] = true;
    }
    if (count == 1) { positions[1] = 0; mask[1] = false; }
    int64_t shape[2] = {1, (int64_t)length};
    int64_t marker_shape[2] = {1, (int64_t)width}, type_shape[2] = {1, 1};
    OrtValue *values[5] = {NULL}, *hidden = NULL, *outputs[2] = {NULL};
    OrtStatus *error = NULL;
    const OrtApi *api = owner->api;
#define TENSOR(index, data, bytes, dims, kind) do { \
    error = api->CreateTensorWithDataAsOrtValue(owner->budget.info, (void *)(data), \
        (bytes), (dims), 2, (kind), &values[index]); if (error) goto finish; } while (0)
    TENSOR(0, ids, length * sizeof(int64_t), shape, ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64);
    TENSOR(1, attention, length * sizeof(int64_t), shape, ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64);
    const char *encoder_names[] = {"input_ids", "attention_mask"};
    const char *encoder_output[] = {"last_hidden_state"};
    error = api->Run(owner->encoder, owner->run, encoder_names, (const OrtValue *const *)values,
                     2, encoder_output, 1, &hidden);
    if (error || atomic_load(&owner->cancelled)) goto finish;
    api->ReleaseValue(values[0]); values[0] = NULL;
    TENSOR(0, positions, width * sizeof(int64_t), marker_shape, ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64);
    TENSOR(2, mask, width * sizeof(bool), marker_shape, ONNX_TENSOR_ELEMENT_DATA_TYPE_BOOL);
    TENSOR(3, &qtype, sizeof(qtype), type_shape, ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64);
    const char *head_names[] = {"hidden_states", "marker_pos", "marker_mask", "qtype", "attention_mask"};
    const OrtValue *head_values[] = {hidden, values[0], values[2], values[3], values[1]};
    const char *head_outputs[] = {"logits", "act_logits"};
    error = api->Run(owner->head, owner->run, head_names, head_values, 5, head_outputs, 2, outputs);
    if (!error && !atomic_load(&owner->cancelled)) {
        float *data = NULL;
        OrtTensorTypeAndShapeInfo *shape_info = NULL;
        size_t elements = 0;
        ONNXTensorElementDataType kind;
        error = api->GetTensorTypeAndShape(outputs[0], &shape_info);
        if (!error) error = api->GetTensorShapeElementCount(shape_info, &elements);
        if (!error) error = api->GetTensorElementType(shape_info, &kind);
        if (shape_info) api->ReleaseTensorTypeAndShapeInfo(shape_info);
        if (!error && (elements != width || kind != ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT))
            error = api->CreateStatus(ORT_INVALID_ARGUMENT, "Invalid Laya output shape");
        if (!error) error = api->GetTensorMutableData(outputs[0], (void **)&data);
        if (!error) memcpy(logits, data, count * sizeof(float));
    }
finish:
    for (size_t i = 0; i < 5; ++i) if (values[i]) api->ReleaseValue(values[i]);
    if (hidden) api->ReleaseValue(hidden);
    for (size_t i = 0; i < 2; ++i) if (outputs[i]) api->ReleaseValue(outputs[i]);
    return status(owner, error);
#undef TENSOR
}

uint64_t hide_system_one_used(const HideSystemOne *owner) { return atomic_load(&owner->budget.used); }
uint64_t hide_system_one_peak(const HideSystemOne *owner) { return atomic_load(&owner->budget.peak); }

uint64_t hide_system_one_free(HideSystemOne *owner) {
    if (!owner) return 0;
    const OrtApi *api = owner->api;
    if (owner->head) api->ReleaseSession(owner->head);
    if (owner->encoder) api->ReleaseSession(owner->encoder);
    if (owner->run) api->ReleaseRunOptions(owner->run);
    if (owner->options) api->ReleaseSessionOptions(owner->options);
    if (owner->env) api->ReleaseEnv(owner->env);
    if (owner->budget.info) api->ReleaseMemoryInfo(owner->budget.info);
    uint64_t remaining = atomic_load(&owner->budget.used);
    free(owner);
    return remaining;
}
