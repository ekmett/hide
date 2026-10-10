#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
"""Extract VileR's original IBM VGA bitmaps; requires fonttools==4.61.1.

Usage: python tools/convert-font.py oldschool_pc_font_pack_v2.2_FULL.zip
See assets/fonts/README.md for provenance and reproduction instructions.
"""

import hashlib
import io
from pathlib import Path
import sys
import zipfile

from fontTools.ttLib import TTFont

archive = Path(sys.argv[1]).read_bytes()
if hashlib.sha256(archive).hexdigest() != "21b3c0a3770ef0afc46564760613d7b078f4fcc9ed93db4b829a440b68822e08":
    raise SystemExit("Expected the official Oldschool PC Font Pack v2.2 FULL archive")
out = Path(__file__).resolve().parents[1] / "assets" / "fonts"
out.mkdir(parents=True, exist_ok=True)
with zipfile.ZipFile(io.BytesIO(archive)) as pack:
    original = pack.read("otb - Bm (linux bitmap)/BmPlus_IBM_VGA_8x16.otb")
    font = TTFont(io.BytesIO(original))
    strike = font["EBLC"].strikes[0]
    assert strike.bitmapSizeTable.bitDepth == 1
    glyphs = font["EBDT"].strikeData[0]
    metrics = {name: table.metrics for table in strike.indexSubTables
               if hasattr(table, "metrics") for name in table.names}
    lines = []
    for code, name in sorted(font.getBestCmap().items()):
        bitmap = glyphs[name]
        metric = metrics[name] if name in metrics else bitmap.metrics
        x = metric.horiBearingX if name in metrics else metric.BearingX
        y = 12 - (metric.horiBearingY if name in metrics else metric.BearingY)
        assert x >= 0 and metric.width + x <= 8 and y >= 0 and y + metric.height <= 16
        rows = [0] * 16
        for row in range(metric.height):
            rows[y + row] = bitmap.getRow(row, metrics=metric)[0] >> x
        lines.append(f"{code:04X}:" + bytes(rows).hex().upper() + "\n")
    (out / "ibm-vga-8x16.hex").write_text("".join(lines), encoding="ascii")
    (out / "IBM-LICENSE.txt").write_bytes(pack.read("LICENSE.TXT"))
    (out / "IBM-README.txt").write_bytes(pack.read("README.TXT"))
    print(f"Extracted {len(lines)} glyphs; source OTB SHA256: {hashlib.sha256(original).hexdigest()}")
