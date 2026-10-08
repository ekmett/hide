/* SPDX-License-Identifier: BSD-3-Clause */
#include "../cbits/shaders/power-mode.h"
#include <assert.h>
#include <stdio.h>

int main(void) {
    struct HidePowerMode state={0};
    float bursts[HIDE_POWER_BURSTS][4];
    float shake[2];
    hide_power_shake(&state,0,shake);
    assert(shake[0]==0 && shake[1]==0);
    assert(!hide_power_pack(&state,0,bursts));
    for (int i=0;i<HIDE_POWER_BURSTS;++i) assert(bursts[i][2]<0);
    hide_power_emit(&state,12,8,100);
    hide_power_shake(&state,99,shake);
    assert(shake[0]==0 && shake[1]==0);
    int moved=0;
    for (unsigned age=0;age<HIDE_POWER_SHAKE_MS;++age) {
        hide_power_shake(&state,100+age,shake);
        assert(shake[0]>=-3.f && shake[0]<=3.f && shake[1]>=-2.f && shake[1]<=2.f);
        moved+=shake[0]!=0 || shake[1]!=0;
    }
    assert(moved>0);
    hide_power_shake(&state,100+HIDE_POWER_SHAKE_MS,shake);
    assert(shake[0]==0 && shake[1]==0);
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
    hide_power_shake(&state,300,shake); // A later impulse restarts an earlier settled shake.
    assert(shake[0]!=0 || shake[1]!=0);
    hide_power_shake(&state,400,shake);
    assert(shake[0]==0 && shake[1]==0 && hide_power_pack(&state,400,bursts)==HIDE_POWER_BURSTS);
    assert(!hide_power_pack(&state,240+HIDE_POWER_LIFETIME_MS,bursts));
    hide_power_shake(&state,240+HIDE_POWER_LIFETIME_MS,shake);
    assert(shake[0]==0 && shake[1]==0);
    state=(struct HidePowerMode){0};
    assert(!hide_power_pack(&state,240,bursts));
    hide_power_shake(&state,240,shake);
    assert(shake[0]==0 && shake[1]==0);
    puts("Power Mode: bounded ring/translation, burst coordinates, seed, settling, expiry and clear pass");
}
