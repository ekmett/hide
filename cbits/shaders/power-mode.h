/* SPDX-License-Identifier: BSD-3-Clause */
#ifndef HIDE_POWER_MODE_H
#define HIDE_POWER_MODE_H
#define HIDE_POWER_BURSTS 4
#define HIDE_POWER_PARTICLES 8
#define HIDE_POWER_LIFETIME_MS 600
#define HIDE_POWER_SHAKE_MS 180

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
/* Latest typing impulse: bounded logical-pixel translation, settled before the
 * sparks expire. Triangle waves avoid per-fragment trigonometry or heap work. */
static inline void hide_power_shake(const struct HidePowerMode *state,uint64_t now,float out[2]) {
    out[0]=out[1]=0;
    const struct HidePowerBurst *burst=&state->bursts[(state->serial-1u)%HIDE_POWER_BURSTS];
    if (!burst->seed || now<burst->started || now-burst->started>=HIDE_POWER_SHAKE_MS) return;
    unsigned age=(unsigned)(now-burst->started);
    float decay=1.f-(float)age/HIDE_POWER_SHAKE_MS;
    float x=(float)((age+burst->seed*17u)%64u)/32.f;
    float y=(float)((age+burst->seed*29u)%80u)/40.f;
    out[0]=3.f*decay*decay*(2.f*(x<=1.f?x:2.f-x)-1.f);
    out[1]=2.f*decay*decay*(2.f*(y<=1.f?y:2.f-y)-1.f);
}
#endif
#endif
