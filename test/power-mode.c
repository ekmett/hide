/* SPDX-License-Identifier: BSD-3-Clause */
#include "../cbits/shaders/power-mode.h"
#include <assert.h>
#include <stdio.h>

int main(void) {
    struct HidePowerMode state={0};
    float bursts[HIDE_POWER_BURSTS][4];
    assert(!hide_power_pack(&state,0,bursts));
    for (int i=0;i<HIDE_POWER_BURSTS;++i) assert(bursts[i][2]<0);
    hide_power_emit(&state,12,8,100);
    assert(hide_power_pack(&state,100,bursts)==1);
    assert(bursts[0][0]==12.5f && bursts[0][1]==8.5f && bursts[0][2]==0);
    assert(hide_power_pack(&state,99,bursts)==0);
    assert(hide_power_pack(&state,100+HIDE_POWER_LIFETIME_MS-1,bursts)==1);
    assert(hide_power_pack(&state,100+HIDE_POWER_LIFETIME_MS,bursts)==0);
    for (int i=0;i<HIDE_POWER_BURSTS*3;++i) hide_power_emit(&state,i,8,200+i);
    assert(hide_power_pack(&state,240,bursts)==HIDE_POWER_BURSTS);
    for (int i=0;i<HIDE_POWER_BURSTS;++i) {
        assert(bursts[i][0]>=8.5f && bursts[i][0]<=11.5f);
        assert(bursts[i][2]>0 && bursts[i][2]<0.1f);
        for (int j=0;j<i;++j) assert(bursts[i][3]!=bursts[j][3]);
    }
    assert(!hide_power_pack(&state,240+HIDE_POWER_LIFETIME_MS,bursts));
    state=(struct HidePowerMode){0};
    assert(!hide_power_pack(&state,240,bursts));
    puts("Power Mode: bounded ring, burst coordinates, seed, expiry and clear pass");
}
