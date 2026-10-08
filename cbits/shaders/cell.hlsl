// SPDX-License-Identifier: BSD-3-Clause
#include "cell.h"
#include "power-mode.h"
Texture2D<float4> glyphAtlas : register(t0, space2);
SamplerState glyphSampler : register(s0, space2);
#ifdef HIDE_WEBGL
Texture2D<uint4> cellData : register(t1, space2);
#else
StructuredBuffer<HideGlyphCell> cells : register(t1, space2);
#endif
cbuffer Display : register(b0, space3) {
    float4 grid; // columns, rows, atlas dimension, unused
    float4 caretMouse; // cursor x/y, mouse x/y; -1 disables either
    float4 viewport; // width, height, CRT enabled, physical pixels per font row
#ifndef HIDE_WEBGL
    float4 powerMode; // live burst count, logical cell height, unused, unused
    float4 powerBursts[HIDE_POWER_BURSTS]; // cell x/y, seconds since typing, seed
#endif
};
HideGlyphCell readCell(uint index) {
#ifdef HIDE_WEBGL
    uint2 position = uint2((index % uint(grid.x)) * 2, index / uint(grid.x));
    HideGlyphCell cell;
    cell.geometry = cellData.Load(int3(position, 0));
    cell.paint = cellData.Load(int3(position + uint2(1, 0), 0));
    return cell;
#else
    return cells[index];
#endif
}
float3 rgb(uint packed) { return float3((packed >> 16) & 255, (packed >> 8) & 255, packed & 255) / 255.0; }
uint rgbWord(float3 value) {
    uint3 channels = uint3(floor(saturate(value) * 255.0 + 0.5));
    return (channels.r << 16) | (channels.g << 8) | channels.b;
}
uint mouseColor(uint value) {
    static const uint colors[16] = {0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,
        0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff};
    for (uint i = 0; i < 16; ++i) if (value == colors[i]) return colors[i ^ 7];
    return value ^ 0xaaaaaa;
}
#ifndef HIDE_WEBGL
uint particleHash(uint value) {
    value ^= value >> 16; value *= 0x7feb352du;
    value ^= value >> 15; value *= 0x846ca68bu;
    return value ^ (value >> 16);
}
float4 powerSparks(float2 position) {
    float4 sparks = 0; // premultiplied color and coverage
    // Keep menu/status chrome untouched. Native input supplies only bounded
    // caret metadata; remote typing feedback uses the last displayed position.
    if (powerMode.x == 0 || position.y < 1 || position.y >= grid.y - 1) return sparks;
    for (uint burst = 0; burst < HIDE_POWER_BURSTS; ++burst) {
        float4 emission = powerBursts[burst];
        float age = emission.z;
        float2 delta = (position - emission.xy) * float2(8, powerMode.y);
        // Conservative logical-pixel bounds avoid particle work elsewhere.
        if (age < 0 || abs(delta.x) > 64 || abs(delta.y) > 64) continue;
        float fade = 1 - age * (1000.0 / HIDE_POWER_LIFETIME_MS);
        for (uint particle = 0; particle < HIDE_POWER_PARTICLES; ++particle) {
            uint seed = particleHash(uint(emission.w) * 31u + particle);
            float2 velocity = float2((float(seed & 1023u) / 511.5 - 1) * 80,
                -40 - float((seed >> 10) & 1023u) * 0.06);
            float2 center = velocity * age + float2(0, 80) * age * age;
            // A short trail follows the instantaneous velocity. Wider colored
            // cores and travel beyond the caret make this readable as sparks.
            float2 trail = -(velocity + float2(0, 160) * age) * 0.04;
            float2 offset = delta - center;
            float along = saturate(dot(offset, trail) / max(dot(trail, trail), 0.01));
            float2 distance = abs(offset - trail * along);
            float ink = saturate(2.5 - distance.x - distance.y) * fade * (1 - 0.65 * along);
            float3 tint = particle % 3 == 0 ? float3(1, 0.65, 0.12) :
                particle % 3 == 1 ? float3(0.15, 0.85, 1) : float3(1, 0.25, 0.65);
            sparks = float4(tint * ink, ink) + sparks * (1 - ink);
        }
    }
    return sparks;
}
#endif
float4 main(float4 color : TEXCOORD0, float2 uv : TEXCOORD1) : SV_Target0 {
    float2 position = uv * grid.xy;
    uint2 cellPosition = uint2(floor(position));
    float2 within = frac(position);
    uint index = cellPosition.y * uint(grid.x) + cellPosition.x;
    float3 result = 0;
    float remaining = 1;
    for (uint depth = 0; depth < 16 && remaining > 0; ++depth) {
        HideGlyphCell cell = readCell(index);
        uint2 origin = uint2(cell.geometry.x & 65535, cell.geometry.x >> 16);
        uint2 extent = uint2(cell.geometry.y & 65535, cell.geometry.y >> 16);
        uint fullCells = cell.geometry.z & HIDE_CELL_WIDTH_MASK;
        uint script = (cell.geometry.z >> HIDE_CELL_SCRIPT_SHIFT) & HIDE_CELL_SCRIPT_MASK;
        uint natural = (cell.geometry.z >> HIDE_CELL_NATURAL_SHIFT) & HIDE_CELL_SCRIPT_MASK;
        uint offset = cell.geometry.z >> 16;
        if (fullCells == 0) break;
        float2 source = float2((float(offset) + within.x) / float(fullCells), within.y);
        bool visibleInk = true;
        if (script != 0) {
            float band = script == 2 ? 0.5 : 0;
            visibleInk = within.x < float(natural) * 0.5 && within.y >= band && within.y < band + 0.5;
            source = float2(within.x * 2 / float(natural), (within.y - band) * 2);
        }
        float2 glyphUV = (float2(origin) + source * float2(extent)) / grid.z;
        float4 ink = visibleInk ? glyphAtlas.SampleLevel(glyphSampler, glyphUV, 0) : float4(0, 0, 0, 0);
        float3 foreground = (cell.paint.z & 2) ? rgb(cell.paint.x) : ink.rgb;
        float3 tile = foreground * ink.a;
        float alpha = ink.a;
        // Lines belong to cell paint, sharing the glyph's ordinary draw and clip.
        // Round bands to physical pixels. In 8-row mode a pixel center lies
        // exactly on font row15; interpolated UV rounding must not erase it.
        float cellPixels = viewport.y / grid.y;
        float pixelRow = floor(within.y * cellPixels);
        bool underline = pixelRow >= floor(15 * cellPixels / 16) && pixelRow < cellPixels;
        bool strike = pixelRow >= floor(7 * cellPixels / 16) && pixelRow < ceil(8 * cellPixels / 16);
        if (((cell.paint.z & 8) && underline) || ((cell.paint.z & 16) && strike)) {
            tile = rgb(cell.paint.x);
            alpha = 1;
        }
        if (cell.paint.z & 1) { tile += rgb(cell.paint.y) * (1 - alpha); alpha = 1; }
        result += tile * remaining;
        remaining *= 1 - alpha;
        if (cell.geometry.w == 0) break;
        index = cell.geometry.w - 1;
    }
    uint value = rgbWord(result);
    if (caretMouse.x >= 0 && all(cellPosition == uint2(caretMouse.xy)) && within.y >= 0.875) value ^= 0xffffff;
    if (caretMouse.z >= 0 && all(cellPosition == uint2(caretMouse.zw))) value = mouseColor(value);
    float3 painted = rgb(value);
#ifndef HIDE_WEBGL
    // Source-over preserves colored particles over bright backgrounds; additive
    // light disappears on white paint and only recolors existing dark glyphs.
    float4 sparks = powerSparks(position);
    painted = painted * (1 - sparks.a) + sparks.rgb;
#endif
    if (viewport.z != 0) {
        float2 n = uv * 2 - 1;
        float radius = dot(n, n) / 2;
        if (viewport.w >= 2 && fmod(floor(uv.y * viewport.y) + 1, viewport.w) < 1) painted *= 1 - 24.0 / 255.0;
        painted *= 1 - (100.0 / 255.0) * radius * radius;
    }
    return float4(painted, 1);
}
