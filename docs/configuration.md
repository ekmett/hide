# Configuration

Set the way you want the editor to open once, then launch it with a file or project path. The shared Turbo Haskell configuration file is `$XDG_CONFIG_HOME/thc/config.toml`, or `~/.config/thc/config.toml` when `XDG_CONFIG_HOME` is unset.

```toml
[editor.defaults]
backend = "metal"
scale = 1.5
screenMode = 259
columns = 120
rows = 50
appearance = "dark"
wordStar = false
blinkCursor = true
crtFilter = true
pixelateUnicode = true
materialIcons = false
streamerMode = false
```

Use `terminal`, `auto`, `metal`, `vulkan`, or `web` for the display backend. `remote` serves the protocol over standard input/output. Scale runs from 1 to 8 in eighth steps. Screen mode 3 defaults to 80×25; 259 defaults to 80×50. Explicit columns and rows override those dimensions. The terminal takes its size from the terminal window.

Command-line options override environment variables, which override project defaults, which override global defaults. For example, `THC_EDIT_BACKEND=web` overrides `backend = "metal"`, and `--terminal` overrides both. `--mode` selects its usual dimensions unless `--size` is also supplied. `--no-crt`, `--classic-icons`, `--standard-keys`, `--no-blink-cursor` and `--no-pixelate-unicode` override enabled defaults.

Startup defaults apply when a session is created. Resuming keeps that session's editing state; the frontend can still use your chosen backend and scale. To change a running session, use **Options > Preferences**. A guest can read and update non-agent settings through `editor_settings`; its `defaults` object updates the startup section without changing the current session.

## Project configuration

Put `thc.toml` in the project root. It uses the same namespaces as the global
configuration and can live in version control:

```toml
[editor.defaults]
screenMode = 259
columns = 120
appearance = "dark"

[editor.agent]
context = """
This package implements the editor. Keep the public API small.
Run the editor checks for changes to input, layout or rendering.
"""
```

Discovery starts at the named file's directory, the named directory, or the
current directory when no path is given. The nearest `thc.toml` wins. Without
one, the search stops at a Cabal package, `cabal.project`, or Git root; the UI
creates `thc.toml` there. Without a project marker, it uses the starting directory.
Only one project file is loaded; parent projects are not recursively merged.

`[editor.defaults]` is merged key by key over global defaults. Omitted values
inherit; project `false` overrides global `true`. Project defaults apply at
startup. The guest's `editor_settings.defaults` writer still edits the global
file; it does not rewrite the project file.

## Agent context

Use `[editor.agent].context` in either file for guidance you want supplied to
conversation agents. Global text comes first, then project text. The project
text adds to the global context; an empty project string adds nothing.

**Options > Agent Context** selects **Global** or **Project**, then **Edit** opens
the TOML in a normal editor window with save, undo and multiline editing. Use
triple-quoted TOML strings as above. Save with F2 before sending your next query
or steering message. Guidance is limited to 16,384 characters per scope.

The first query after connecting or resuming supplies the skill catalog and
current context. Later queries and steering messages resend context only when
its saved value changes, including when it is cleared. Queued queries use the
context current when dispatched. Editing context does not submit a query or
interrupt a running response by itself. Invalid configuration stops submission
and retains the query for repair.

Agents can read the effective text and its source paths through `agent_settings`.
They cannot edit this configuration through guest file/input tools. Context is
sent to the provider, so use it for guidance rather than credentials. Tool
permissions remain global: a project file cannot enable tools or override
**Agent Permissions**.

## Streamer mode

Turn on **Options > Preferences > Streamer mode** before sharing your screen,
or start with `--streamer`. Sensitive input values and session keys are masked
on every frontend; their stored values are unchanged. `--no-streamer` turns it
off. This is a user-controlled preference: guests can read its state but cannot
change it through input or settings tools.

Guest views always redact sensitive values, whether or not Streamer mode is on.
Source files are not scanned for secrets by this display option.

## Agent Permissions

**Options > Agent Permissions** saves each tool's Enable, Prompt or Disable policy:

```toml
[editor.mcp.permissions]
read_buffer = "enable"
buffer_apply_diff = "prompt"
terminal_start = "disable"
```

Read-only tools default to Enable; other tools default to Prompt. Omitted tools retain that default. See [session tools](session-tools.md) for the available services.

The editor preserves unrelated settings and comments when saving its own entries. Additional compiler sections can share this file as they are introduced.
