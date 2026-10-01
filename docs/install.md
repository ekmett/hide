# Installation

Build the frontend you want to use. The editor is a standalone executable;
editing, HLS, Git and conversations do not require the THC compiler or runtime.

## Build the editor

You need GHC 9.6 or newer, Cabal, `pkg-config` and the development files for
utf8proc 2.10 or newer. On macOS:

```sh
brew install utf8proc
```

On Debian/Ubuntu, the package is `libutf8proc-dev`; check that your distribution
supplies the required version. Then clone the `main` branch and build:

```sh
git clone --branch main https://github.com/ekmett/thc-edit.git
cd thc-edit
cabal build all
cabal run thc-edit -- .
```

The default build runs in a UTF-8 terminal. An 80-column, 25-row terminal is a
useful starting size. Mouse support, modified keys and exact colors follow the
terminal's capabilities.

To install on your path:

```sh
cabal install exe:thc-edit --installdir="$HOME/.local/bin"
```

Add that directory to `PATH` if it is not already there. All examples using
`thc-edit` also work from a checkout as `cabal run thc-edit -- ...`, with the
appropriate build flags before `thc-edit`.

## Native window

Enable the `window` flag and install SDL3 3.2 or newer. On macOS:

```sh
brew install sdl3
cabal run -fwindow thc-edit -- --window .
```

`--window` chooses Metal on macOS and Vulkan elsewhere. `--metal` and `--vulkan`
select them explicitly. Linux window builds also need Pango/Cairo development
headers (`libpango1.0-dev` on Debian/Ubuntu). `fonts-noto-color-emoji` supplies
color emoji where a fallback font is needed. macOS uses CoreText.

To keep this frontend in an installed build:

```sh
cabal install exe:thc-edit -fwindow --installdir="$HOME/.local/bin"
```

## Browser

The browser frontend needs the `web` flag and no SDL installation:

```sh
cabal run -fweb thc-edit -- --web .
```

It opens a WebGL page served by the editor on an ephemeral loopback port. Keep
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
| Run a Haskell program through THC | A current THC executable plus the terminal build; see [running](running.md) |
| Remote editing | `thc-edit` on both machines; see [remote editing](remote.md) |

Sessions and SSH editing are part of the default build. The optional flags select
native windows (`-fwindow`), the browser (`-fweb`) and embedded terminals
(`-fterminal`) independently.

## Embedded terminal

The optional `terminal` flag enables **File > Terminal**, **Run > Run** and ACP
terminal requests through libghostty-vt. The current PTY backend requires POSIX.
It works independently of SDL.

The Ghostty C API is evolving. The verified source revision is
`76895d97b74ff6b24c2b1543bcd69ccc18048a4d`, built with Zig 0.16.0:

```sh
git clone https://github.com/ghostty-org/ghostty /tmp/thc-ghostty
cd /tmp/thc-ghostty
git checkout 76895d97b74ff6b24c2b1543bcd69ccc18048a4d
zig build -Demit-lib-vt=true -Demit-xcframework=false -Doptimize=ReleaseFast --prefix /tmp/thc-ghostty-install
cd /path/to/thc-edit
export PKG_CONFIG_PATH=/tmp/thc-ghostty-install/share/pkgconfig:$PKG_CONFIG_PATH
cabal run -fwindow -fterminal --ghc-options=-optl-Wl,-rpath,/tmp/thc-ghostty-install/lib thc-edit -- --window .
```

Keep the installed Ghostty library available at runtime, or use your system's
normal library installation path. A build without `terminal` can still edit,
use HLS, review Git changes and hold conversations.

## Defaults

| Variable | Purpose |
| --- | --- |
| `THC_EDIT_BACKEND` | `terminal`, `auto`, `metal`, `vulkan` or `web` |
| `THC_EDIT_SCALE` | Pixel scale from 1 to 8, rounded to 1/8 steps |
| `THC_EDIT_APPEARANCE` | `light`, `dark` or `system` |
| `THC_EDIT_WEB_OPEN` | Set to `0` to print the browser URL without opening it |
| `THC_EDIT_HLS` | Alternate HLS executable |
| `THC_ROOT` | Default source/build root in Run > Target |

Explicit frontend, scale and appearance options override these defaults.
For example, `thc-edit --terminal .` overrides a configured graphical frontend.
`thc-edit --help` lists the command-line options. [Display and frontends](display.md)
covers screen modes and interactive preferences.

Provider and Run target settings are stored in the user's `thc-edit`
configuration directory, normally `$XDG_CONFIG_HOME/thc-edit` or
`~/.config/thc-edit` on Unix. Appearance and key preferences can be changed for
the running editor under **Options > Preferences**; use startup flags or the
environment variables above for repeatable launches.
