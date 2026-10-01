"""Convert the bundled Material Design SVGs to two-cell VGA bitmap tiles.
Requires rsvg-convert and ImageMagick only when regenerating the asset.
"""
from pathlib import Path
import subprocess

root = Path(__file__).resolve().parent.parent
rows = []
for name, code in [('folder', 0xF024B), ('folder-open', 0xF0770)]:
    png = subprocess.check_output(['rsvg-convert', '-w', '64', '-h', '64', str(root / 'assets/icons' / (name + '.svg'))])
    alpha = subprocess.check_output(['magick', 'png:-', '-alpha', 'extract', '-filter', 'box', '-resize', '16x16!', '-depth', '8', 'gray:-'], input=png)
    assert len(alpha) == 256
    bits = [sum((alpha[y*16+x] >= 128) << (15-x) for x in range(16)) for y in range(16)]
    rows.append(f'{code:05X}:' + ''.join(f'{row:04X}' for row in bits))
(root / 'assets/fonts/material-icons.hex').write_text('\n'.join(rows) + '\n')
