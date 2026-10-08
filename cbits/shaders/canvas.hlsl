// SPDX-License-Identifier: BSD-3-Clause
// A surface owns only its normal-cell stencil slots; image sampling never
// changes overlap, wide-glyph clipping, or input authority.
Texture2D<float4> canvasImage : register(t0, space2);
SamplerState canvasSampler : register(s0, space2);
#ifdef HIDE_WEBGL
Texture2D<uint4> canvasMask : register(t1, space2);
#else
StructuredBuffer<uint> canvasMask : register(t1, space2);
#endif
cbuffer Canvas : register(b0, space3) {
    float4 canvasGrid; // columns, rows, surface slot, unused
    float4 canvasTarget; // x/y/width/height in character cells
};
float4 main(float4 color : TEXCOORD0, float2 uv : TEXCOORD1) : SV_Target0 {
    float2 position = uv * canvasGrid.xy;
    uint2 cell = uint2(floor(position));
#ifdef HIDE_WEBGL
    uint owner = canvasMask.Load(int3(cell, 0)).x;
#else
    uint owner = canvasMask[cell.y * uint(canvasGrid.x) + cell.x];
#endif
    if ((owner & 32767u) != uint(canvasGrid.z)) discard;
    float2 imageUV = (position - canvasTarget.xy) / canvasTarget.zw;
    float3 painted = 0;
    if (all(imageUV >= 0) && all(imageUV < 1)) {
        float4 sample = canvasImage.SampleLevel(canvasSampler, imageUV, 0);
        painted = sample.rgb * sample.a; // straight RGBA over the black canvas
    }
    if (owner & 32768u) painted *= 0.5;
    return float4(painted, 1);
}
