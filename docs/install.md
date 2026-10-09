<!-- SPDX-FileCopyrightText: 2026 Edward Kmett
SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 -->

# Installation

All supported frontends are enabled by default. The editor is a standalone executable;
editing, HLS, Git and conversations do not require the THC compiler or runtime.

## Build the editor

You need GHC 9.6 or newer, Cabal, `pkg-config` and the development files for
utf8proc 2.10 or newer, SDL3 3.4 or newer, and libghostty-vt.
Set up Ghostty using the [embedded terminal instructions](#embedded-terminal)
below before building, or omit it with `-f-terminal`. On macOS:

```sh
brew install utf8proc sdl3
```

On Debian/Ubuntu, the package is `libutf8proc-dev`; check that your distribution
supplies the required version. Then clone the `main` branch and build:

```sh
git clone --branch main https://github.com/ekmett/hide.git
cd hide
cabal build all
cabal run hide -- .
```

The default build includes terminal, native-window and browser displays.
Without a display option, the editor runs in a UTF-8 terminal. An 80-column, 25-row terminal is a
useful starting size. Mouse support, modified keys and exact colors follow the
terminal's capabilities.

To install on your path, including the `th` and `thc edit` launchers:

```sh
make install
```

`make install` puts `hide`, `th` and `thc-edit` in `~/.local/bin` (override
`BINDIR` to choose another location). The small launchers find `hide` beside
themselves and forward every argument. With THC installed, its external-command
dispatch makes `thc edit` work. hide itself does not require THC.
For a binary-only install, use `cabal install exe:hide`; copy the launchers
from `bin/` beside it if wanted. Windows uses `th.cmd` and `thc-edit.cmd` beside
`hide.exe`.

Add that directory to `PATH` if it is not already there. All examples using
`hide` also work from a checkout as `cabal run hide -- ...`, with the
appropriate build flags before `hide`.

## Native window

Native windows are enabled by default. Install SDL3 3.4 or newer. Packaged Metal and Vulkan shaders are included; DXC and SPIRV-Cross are needed only when regenerating the HLSL shader assets. On macOS:

```sh
brew install sdl3
cabal run hide -- --window .
```

`--window` chooses Metal on macOS and Vulkan elsewhere. `--metal` and `--vulkan`
select them explicitly. Linux window builds also need Pango/Cairo development
headers (`libpango1.0-dev` on Debian/Ubuntu). `fonts-noto-color-emoji` supplies
color emoji where a fallback font is needed. macOS uses CoreText.

An installed build includes this frontend too:

```sh
cabal install exe:hide --installdir="$HOME/.local/bin"
```

## Browser

The browser frontend is enabled by default:

```sh
cabal run hide -- --web .
```

It opens a WebGL2 page served by the editor on an ephemeral loopback port. Keep
the frontend process running while using the page. The editor session runs
separately, so you can detach and [resume it later](sessions.md). Set `THC_EDIT_WEB_OPEN=0` to print
the URL without opening a browser automatically.

## Optional tools

| Work | Install or configure |
| --- | --- |
| Types, definitions, completion and diagnostics | HLS matching the package's GHC; see [Haskell](haskell.md) |
| Repository review and operations | Git on the editor host's `PATH` |
| Conversations | An ACP stdio provider under Options > Agents; see [conversations](conversations.md) |
| Shells and program output | The `terminal` build described below |
| Compile, build, or run Haskell | THC, or GHC and Cabal; add the terminal build for interactive programs. See [running](running.md). |
| Remote editing | `hide` on both machines; see [remote editing](remote.md) |

Build flags are opt-out: `-f-window` omits SDL/native windows, `-f-web` omits
the browser server, and `-f-terminal` omits Ghostty/embedded terminals. For a
minimal terminal-display or remote-server build:

```sh
cabal build all -f-window -f-web -f-terminal
```

This still includes sessions, SSH editing, HLS, Git and conversations. A
browser-only build can use `-f-window -f-terminal`, avoiding both native libraries.

## Matching Haskell debugger

When a selected GHC has no available hdb adapter, the editor can offer a matching
[official hdb binary](https://github.com/well-typed/haskell-debugger/releases).
Accept the concrete download offer to fetch it in the background. The Downloads
window shows progress, failures and cancellation; declining downloads nothing.
There is no automatic build from source.

The current catalog contains hdb 0.14.0.0 for GHC **9.14.1**, on macOS Intel/Apple
Silicon and the published Linux distributions. Other exact GHC versions,
unlisted platforms and Windows currently require a separately installed adapter.
The editor verifies the pinned SHA256 and retains the upstream wrapper, which
checks the selected GHC's version and boot-library ABI before reporting ready.
A matching version number alone does not guarantee ABI compatibility.

Tools share THC's CBD storage root: an absolute `THC_CACHE_HOME` override,
`~/Library/Caches/thc` on macOS, or the platform XDG cache directory's `thc`
subdirectory elsewhere. Versioned launchers live in `bin/hdb-9.14.1`; complete
bundles live under `tools/hdb/<release>/<platform>/`. Existing unmanaged launchers
are never overwritten. These tools can be downloaded again if the cache is
cleared. Acquisition needs `curl`, `tar`, and `shasum` on macOS or `sha256sum` on
Linux; it does not change shell startup files or `PATH`.

## Bundled Ghostty discovery

On macOS and Linux, the checked-in `cabal.project` selects a small pkg-config
wrapper that searches `.deps/ghostty/share/pkgconfig` and
`.deps/ghostty/lib/pkgconfig` before the inherited search path. Installing
Ghostty under this checkout's `.deps/ghostty` therefore lets ordinary
`cabal build` work from the editor or a fresh shell without exporting
`PKG_CONFIG_PATH`. Run Cabal from the checkout root: the extra program path
is relative to the invocation directory. Other dependencies still use the
system pkg-config paths.
This discovers an existing installation; it does not download or build Ghostty.

`cabal.project.local` is the ignored file for machine-specific Cabal overrides.
Native Windows uses its normal `pkg-config.exe`; it skips the POSIX wrapper.
A Ghostty installation elsewhere, including the Windows setup below, still
needs its pkg-config directory on `PKG_CONFIG_PATH`.

## Embedded terminal

Embedded terminals enable **File > Terminal**, **Run > Run** and ACP
terminal requests through libghostty-vt. They are enabled by default and work
independently of SDL. macOS and Linux use a PTY; native Windows uses ConPTY
(Windows 10 version 1809 or newer). On Windows the default shell is `COMSPEC`,
usually `cmd.exe`; terminal commands run in the editor session’s project directory.

The Ghostty C API is evolving. The verified source revision is
`76895d97b74ff6b24c2b1543bcd69ccc18048a4d`, built with Zig 0.16.0:

```sh
git clone https://github.com/ghostty-org/ghostty /tmp/thc-ghostty
cd /tmp/thc-ghostty
git checkout 76895d97b74ff6b24c2b1543bcd69ccc18048a4d
zig build -Demit-lib-vt=true -Demit-xcframework=false -Doptimize=ReleaseFast --prefix /tmp/thc-ghostty-install
cd /path/to/hide
export PKG_CONFIG_PATH=/tmp/thc-ghostty-install/share/pkgconfig:$PKG_CONFIG_PATH
cabal run --ghc-options=-optl-Wl,-rpath,/tmp/thc-ghostty-install/lib hide -- --window .
```

For a native Windows remote server, build the same pinned Ghostty checkout in
PowerShell, then expose its package metadata and DLL:

```powershell
$ghosttyPrefix = "$env:LOCALAPPDATA\hide\ghostty"
zig build -Demit-lib-vt=true -Demit-xcframework=false -Doptimize=ReleaseFast --prefix $ghosttyPrefix -j4
$env:PKG_CONFIG_PATH = "$ghosttyPrefix\share\pkgconfig;$env:PKG_CONFIG_PATH"
$env:PATH = "$ghosttyPrefix\bin;$env:PATH"
cd C:\path\to\hide
cabal build all -f-window -f-web
```

Keep utf8proc's package metadata and DLL on those paths too. This includes
embedded terminals on the remote server; the native-window and browser clients
can run on another machine. The pinned Ghostty library builds in ReleaseFast;
its upstream `test-lib-vt` checks should use `-Doptimize=Debug`.

Keep the installed Ghostty library available at runtime, or use your system's
normal library installation path. A build with `-f-terminal` can still edit,
use HLS, review Git changes and hold conversations.

## Bash completion

With current `thc` and the installed `thc-edit` launcher on your `PATH`, enable Bash completion with:

```bash
source <(thc --bash-completion-script)
```

This completes `thc edit` options and file paths, including names with spaces.
Add the command to your Bash startup file to enable it in future shells.

## Defaults

| Variable | Purpose |
| --- | --- |
| `THC_EDIT_BACKEND` | `terminal`, `auto`, `metal`, `vulkan` or `web` |
| `THC_EDIT_SCALE` | Pixel scale from 1 to 8, rounded to 1/8 steps |
| `THC_EDIT_APPEARANCE` | `light`, `dark` or `system` |
| `THC_EDIT_WEB_OPEN` | Set to `0` to print the browser URL without opening it |
| `THC_EDIT_HLS` | Alternate HLS executable |
| `THC_ROOT` | Default source/build root in Run > Target |

Environment variables override `[editor.defaults]` in the shared
[configuration file](configuration.md). Explicit frontend, scale and appearance
options override both.
For example, `hide --terminal .` overrides a configured graphical frontend.
`hide --help` lists the command-line options. [Display and frontends](display.md)
covers screen modes and interactive preferences.

Provider and Run target settings remain in the user's legacy `thc-edit`
configuration directory, normally `$XDG_CONFIG_HOME/thc-edit` or
`~/.config/thc-edit` on Unix. Appearance and key preferences can be changed for
the running editor under **Options > Preferences**. Shared startup defaults and
Agent Permissions live separately in `thc/config.toml`; use that file for
repeatable launches across projects.

The rename preserves existing settings and resumable sessions in their
`thc-edit` storage directories. The `THC_EDIT_*` environment variables and
shared `thc/config.toml` / project `thc.toml` settings also retain their names;
none of these require a THC installation.
