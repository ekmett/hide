// SPDX-License-Identifier: BSD-3-Clause
#include "cell.h"
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
        uint fullCells = cell.geometry.z & 65535;
        uint offset = cell.geometry.z >> 16;
        if (fullCells == 0) break;
        float2 glyphUV = (float2(origin) + float2((float(offset) + within.x) / float(fullCells), within.y) * float2(extent)) / grid.z;
        float4 ink = glyphAtlas.SampleLevel(glyphSampler, glyphUV, 0);
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
    if (viewport.z != 0) {
        float2 n = uv * 2 - 1;
        float radius = dot(n, n) / 2;
        if (viewport.w >= 2 && fmod(floor(uv.y * viewport.y) + 1, viewport.w) < 1) painted *= 1 - 24.0 / 255.0;
        painted *= 1 - (100.0 / 255.0) * radius * radius;
    }
    return float4(painted, 1);
}
