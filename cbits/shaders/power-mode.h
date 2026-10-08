/* SPDX-License-Identifier: BSD-3-Clause */
#ifndef HIDE_POWER_MODE_H
#define HIDE_POWER_MODE_H
#define HIDE_POWER_BURSTS 4
#define HIDE_POWER_PARTICLES 8
#define HIDE_POWER_LIFETIME_MS 600

#if defined(__cplusplus) || defined(__STDC__)
#include <stdint.h>
/* SDL-thread state. Fixed storage; time is monotonic milliseconds, coordinates
 * are screen cells. No buffer contents, glyphs or GPU-owned storage are retained. */
struct HidePowerBurst { uint64_t started; int x,y; unsigned seed; };
struct HidePowerMode { struct HidePowerBurst bursts[HIDE_POWER_BURSTS]; unsigned serial; };

static inline void hide_power_emit(struct HidePowerMode *state,int x,int y,uint64_t now) {
    unsigned slot=state->serial++%HIDE_POWER_BURSTS;
    state->bursts[slot]=(struct HidePowerBurst){now,x,y,(state->serial%65535u)+1u};
}
/* Pack the shared float4 array for one actual presentation. Expired entries have
 * negative age; emit/pack never allocate or inspect text. Returns live count. */
static inline unsigned hide_power_pack(const struct HidePowerMode *state,uint64_t now,float out[HIDE_POWER_BURSTS][4]) {
    unsigned active=0;
    for (unsigned i=0;i<HIDE_POWER_BURSTS;++i) {
        const struct HidePowerBurst *burst=&state->bursts[i];
        int live=burst->seed && now>=burst->started && now-burst->started<HIDE_POWER_LIFETIME_MS;
        out[i][0]=(float)burst->x+0.5f; out[i][1]=(float)burst->y+0.5f;
        out[i][2]=live?(float)(now-burst->started)/1000.f:-1.f;
        out[i][3]=(float)burst->seed;
        active+=live;
    }
    return active;
}
#endif
#endif
