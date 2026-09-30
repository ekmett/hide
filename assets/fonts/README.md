# Bundled bitmap fonts

The source framebuffer uses 8 × 16 pixel cells. Mode 3 displays them at
their original aspect ratio; Mode 259 compresses their display height by
half to fit 80 × 50 characters in the same pixel area, using nearest-neighbor
sampling. Both modes use the same unmodified bitmap assets.
IBM VGA glyphs take precedence; GNU
Unifont fills uncovered code points with 8 × 16 or 16 × 16 bitmaps. Unsupported
code points display the IBM replacement glyph. These assets affect display
only: they do not replace characters in the document. There is no runtime
font-library dependency. Bitmap rows are stored in standard Unifont HEX
format (code point, colon, 16 rows in hexadecimal).

## IBM VGA

`ibm-vga-8x16.hex` contains 787 glyphs extracted without resampling from
**BmPlus IBM VGA 8x16**, the original-bitmap counterpart of PxPlus IBM VGA
8x16, in [VileR's Ultimate Oldschool PC Font Pack 2.2](https://int10h.org/oldschool-pc-fonts/).
Copyright © 2016–2020 VileR. Distributed under **CC BY-SA 4.0**; the original
`IBM-LICENSE.txt` and `IBM-README.txt` accompany it. This HEX conversion was
made on 2026-09-30 and is also licensed CC BY-SA 4.0. The only changes are
format conversion and padding the source glyph bounding boxes to their
original 8 × 16 cells; glyph designs and alignment are unchanged.

Source archive:
[oldschool_pc_font_pack_v2.2_FULL.zip](https://int10h.org/oldschool-pc-fonts/download/oldschool_pc_font_pack_v2.2_FULL.zip)

SHA-256:

- Archive: `21b3c0a3770ef0afc46564760613d7b078f4fcc9ed93db4b829a440b68822e08`
- Archive entry `otb - Bm (linux bitmap)/BmPlus_IBM_VGA_8x16.otb`: `c0dd65b2a3cf60f018b185b608d95f460278f39ed2362c5db9809680cc886f8e`
- Generated `ibm-vga-8x16.hex`: `f9c1a55540aa9b6d8f7f0fe8ab9706afd89ee151b9397f5de47f222fd4565323`

Reproduce from the repository root (fontTools is a conversion dependency only):

```sh
python3 -m venv /tmp/thc-font-convert
/tmp/thc-font-convert/bin/pip install fonttools==4.61.1
curl -LO https://int10h.org/oldschool-pc-fonts/download/oldschool_pc_font_pack_v2.2_FULL.zip
/tmp/thc-font-convert/bin/python tools/convert-font.py oldschool_pc_font_pack_v2.2_FULL.zip
```

The converter rejects any archive whose checksum differs. It extracts
embedded monochrome bitmap rows directly, so there is no rasterizer,
hinting, antialiasing, or platform-dependent font rendering step.

## GNU Unifont

`unifont-18.0.01.hex` is the unchanged official GNU Unifont 18.0.01 BMP HEX
file (57,086 glyphs), downloaded from [Unifoundry](https://unifoundry.com/unifont/).
Copyright © 1998–2026 Roman Czyborra, Paul Hardy, Qianqian Fang, Andrew
Miller, Johnnie Weaver, David Corbett, Ælla Chiana Moskopp, Rebecca
Bettencourt, Ho-Seok Ee, et al.

It is dual-licensed under **SIL OFL 1.1** and **GPL 2 or later with the GNU
Font Embedding Exception**. We redistribute under OFL 1.1. The original
distribution notices are in `UNIFONT-LICENSE.txt`, `UNIFONT-OFL-1.1.txt`, and
`UNIFONT-README.txt`; `UNIFONT-COPYRIGHT.txt` transcribes the copyright and
license records from the official font's OpenType name table.

Source files:

- [unifont-18.0.01.hex.gz](https://unifoundry.com/pub/unifont/unifont-18.0.01/font-builds/unifont-18.0.01.hex.gz)
- [unifont-18.0.01.tar.gz](https://unifoundry.com/pub/unifont/unifont-18.0.01/unifont-18.0.01.tar.gz) (original notices and OpenType name table)

SHA-256:

- Compressed HEX: `e66385c79a0b8b24a466f3129930e08a966a935b4bf3b28c6bb17a9df9bf791d`
- Uncompressed HEX: `2ab7801c809b76541a10b25858c1a962bdb650eee56b4c7df1f325b56e3955ee`
- Source archive: `eab60847aac34c8768765cecc7821faf50de2636187b452b9b5fa50a12b00bc3`

Reproduce the fallback asset without a conversion dependency:

```sh
curl -LO https://unifoundry.com/pub/unifont/unifont-18.0.01/font-builds/unifont-18.0.01.hex.gz
gzip -dc unifont-18.0.01.hex.gz > assets/fonts/unifont-18.0.01.hex
```

This bitmap fallback provides individual code point glyphs, without complex
script shaping or supplementary-plane coverage. Those code points remain
intact in the editor buffer even when a replacement glyph is displayed.
