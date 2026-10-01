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
```

Use `terminal`, `auto`, `metal`, `vulkan`, or `web` for the display backend. `remote` serves the protocol over standard input/output. Scale runs from 1 to 8 in eighth steps. Screen mode 3 defaults to 80×25; 259 defaults to 80×50. Explicit columns and rows override those dimensions. The terminal takes its size from the terminal window.

Command-line options override environment variables, which override the configuration file. For example, `THC_EDIT_BACKEND=web` overrides `backend = "metal"`, and `--terminal` overrides both. `--mode` selects its usual dimensions unless `--size` is also supplied. `--no-crt`, `--classic-icons`, `--standard-keys`, `--no-blink-cursor` and `--no-pixelate-unicode` override enabled defaults.

Startup defaults apply when a session is created. Resuming keeps that session's editing state; the frontend can still use your chosen backend and scale. To change a running session, use **Options > Preferences**. A guest can read and update non-agent settings through `editor_settings`; its `defaults` object updates the startup section without changing the current session.

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
