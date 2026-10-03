# Browser backend

Goal: `hide --web` opens the real editor in a local browser, using WebGL,
bundled character tiles, optional CRT filtering, and bidirectional WebSocket input.
The existing Haskell model, effects, and tooling remain authoritative.

- [x] Add optional `web` Cabal flag and `Web` frontend; keep SDL optional.
- [x] Serve only bundled page assets on an ephemeral loopback port. Use a random
  session path and validate WebSocket Origin. Allow one connected client; retain
  the document state through disconnect/reload.
- [x] Send measured grapheme spans, bitmap tiles, cursor and appearance state.
  Encode UTF-8 color runs using per-frame selection between full-screen and row-update DEFLATE, with the previous reconstructed screen as dictionary. Browser canvas supplies native Unicode fallback;
  WebGL presents the texture, DOS mouse cursor, blinking caret and CRT effect.
- [x] Translate keyboard, modifiers, mouse/drag, double click, wheel, paste,
  clipboard, resize and zoom. Fullscreen/Keyboard Lock require an explicit click.
  Losing focus releases modifiers/drag. Do not promise OS-reserved shortcuts.
- [x] Verify protocol parsing, row generation, origin validation, reconnect,
  edits and save, browser rendering and WebGL errors. Rebuild existing tests.
- [x] Document launch, build flag, keyboard limits and browser Unicode differences.
  Commit and launch a browser demo without disturbing existing editor sessions.

Files: `Hide.Web` owns transport and input decoding; `assets/web` owns browser
rendering/input. `Frontend`, `App`, `Font`, and the Cabal file integrate the backend.
The CRT fragment shader matches the native 24/255 scanline darkness and
100/255 radial-squared vignette, with scanlines disabled below two physical
pixels per glyph row. Normal and compressed modes retain their existing metrics.

Review focus: malformed/oversized input, stale connections, Unicode/IME, clipboard
permissions, and fractional/Retina mouse coordinates. No remotely exposed mode.

## Bandwidth trial

The comparison replays the same 131 rendered updates at 80x25, 128x50 and
240x80: typing (including Unicode), cursor motion, 40 lines of scrolling,
menus, and scrolling back. Source: the editor model file from this working
revision. Font/asset setup, client input and TCP/IP overhead are excluded;
server WebSocket framing is included. Each candidate reconstructs the exact
screen and every compressed message is decompressed and checked. Compression
is raw DEFLATE level 8 with 15-bit history and Z_SYNC_FLUSH, matching the
WebSocket library defaults. Three compression runs give median CPU time.

| Screen | Full raw | Full/context | Full/reset | Rows/context | Cells/context |
|---|---:|---:|---:|---:|---:|
| 80x25 | 713761 | 18634 | 160533 | 15581 | 17721 |
| 128x50 | 1629070 | 31745 | 297231 | 27729 | 32595 |
| 240x80 | 3948860 | 168274 | 512563 | 58808 | 73279 |

Values are total bytes. Full/context compression costs 4.06, 7.45 and 38.99 ms
respectively across the entire trace. The 240x80 snapshots range from 28288 to
37340 uncompressed bytes; 25 exceed 32768 bytes. This is a replay of actual
renderer output, not a packet capture or a claim about arbitrary workloads.
Full/context remains the implementation requested; row patches win bandwidth
in this trace, especially when snapshots exceed the dictionary.

Reproduce after building with `-fweb`:

```sh
cabal exec -v0 -- runghc -package=hide tools/web-bandwidth.hs > frames.jsonl
python3 tools/web-bandwidth.py frames.jsonl
```

Raw local results: `.deps/bandwidth-trial/frames.jsonl` and `results.json`.

Final validation: full editor suite and native SDL event checks pass; web-only
build succeeds without SDL/Ghostty. Browser WebGL rendering, Copy and Download
were exercised. A compressed WebSocket fixture imported/downloaded all 256 byte
values unchanged and completed Exit. Native Metal preview visually checked cyan
Help, heading colors, tables, padded blue code and black shell panels. Browser
native-menu Undo/Redo and beforeunload prompts remain browser-dependent; the
model/guard paths are implemented, but no cross-browser claim is made.


## Adaptive frame encoding and appearance

The user requested independently selectable frame formats and Light/Dark/System
appearance. The final protocol uses a one-byte tag followed by raw DEFLATE:
0 resets the dictionary and sends all rows; 1 sends all rows using the previous
screen; 2 sends changed rows using that same dictionary. Metadata carries only
changed keys except on reset. The compressor measures both actual candidate
lengths; ties prefer full frames. No WebSocket compression is layered over it.
The dictionary is the last 32768 bytes of compact UTF-8 JSON for the complete
row array. This schema contains arrays, integer coordinates/colors and strings;
Node checks the Haskell output against the production JavaScript decoder.
The receiver prepends a local nonfinal stored DEFLATE block containing the
dictionary, decodes with DecompressionStream, then discards the prefix output.
This avoids a decoder dependency and never transmits the dictionary.

Replaying the same 131-update trace using actual Haskell packets measured:

| Screen | Adaptive bytes | Previous full/context bytes | Previous rows/context bytes |
|---|---:|---:|---:|
| 80x25 | 25795 | 18634 | 15581 |
| 128x50 | 37984 | 31745 | 27729 |
| 240x80 | 80872 | 168274 | 58808 |

All 130 post-initial updates chose rows on this trace. Both candidates were
independently decoded and checked for every update, including full-frame
candidate switches and screens exceeding 32 KiB. Independent previous-screen
dictionaries lose some cumulative compression history: the adaptive protocol
beats full/context at the large size but does not beat persistent row/context
on this trace. These are measured wire payloads plus WebSocket headers, not
network captures. Assets, input, acknowledgments and TCP/IP are excluded.

Reproduce after generating frames.jsonl with the earlier command:

```
cabal exec -v0 --offline -- runghc -package=hide tools/web-wire-trial.hs < .deps/bandwidth-trial/frames.jsonl > .deps/bandwidth-trial/packets.jsonl
node test/web-wire.mjs .deps/bandwidth-trial/packets.jsonl
```

Light Help is cyan with blue code and gray shell panels; Dark Help is blue with
black code/shell panels. Ghostty default colors change through its API so explicit
ANSI/RGB and OSC overrides remain intact. System follows SDL/browser appearance;
text mode uses COLORFGBG at startup with a dark fallback. Command line and
environment overrides are documented in README.
