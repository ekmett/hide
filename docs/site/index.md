# Turbo Haskell editor

`thc-edit` is a source editor for Haskell projects, written in Haskell. It brings
file editing, language tools, builds, debugging, Git and agent conversations into
one workspace. Use it in a terminal, a native Metal/Vulkan window or a browser,
with the same files, windows and commands in each.

The interface follows the Borland Turbo Pascal editor: menus, overlapping
windows, split views and a status bar with clickable shortcuts. Files, messages,
terminals and conversations can stay open beside the code you are working on.
The editor runs independently of the THC compiler; you can use GHC or THC for
project builds and runs.

[![A recorded agent conversation about the editor’s buffer, replayed with an unsent follow-up.](screenshots/conversation.png)](screenshots/conversation.png)

The screenshots in these guides show the Metal frontend working in this project,
with the CRT filter and Pixelate Unicode enabled. Click an image to view it at
full resolution.

## Open a project

[Install the editor](../install.md), then open a directory or a source file:

```sh
thc-edit .
thc-edit Main.hs
```

Opening a directory shows its files in the Files pane and opens its Cabal package
file when one is present. Add `--window` for a native window or `--web` for the
browser. The default display is the terminal.

Use **F3** to open a file, **F2** to save, **F10** for the menus and **F1** for help.
The [editing guide](../editing.md) covers navigation, search, selection, window
arrangement and shared split views. [Display and frontends](../display.md)
covers appearance, scaling and clipboard support.

## Edit, build and debug

With [Haskell Language Server](../haskell.md), inspect types, follow definitions,
complete identifiers, rename symbols and navigate diagnostics as you edit.
**Compile > Make** builds the selected target; **Run > Run** runs it in an
embedded terminal. The [running and debugging guide](../running.md) covers
THC and GHC targets, tests, breakpoints, stepping and stack inspection.

[Review and commit changes](../git.md) in the editor, or inspect binary files
with the [hex editor](../hex.md).

## Work with agents

Open a [conversation](../conversations.md) beside your source. An agent can work
with live buffers, including unsaved edits, and use language tools, terminals,
builds and the debugger through the editor's [session tools](../session-tools.md).
You control access through **Options > Agent Permissions**.

[Agent skills](../agent-skills.md) describe workflows; the
[operation reference](../agent-tools.md) lists the available calls.
[Configuration](../configuration.md) covers provider setup, project context,
permissions and startup defaults.

## Resume locally or over SSH

Each desktop belongs to a [session](../sessions.md). Detach and return later,
or resume through a different frontend:

```sh
thc-edit --resume
thc-edit --web --resume
```

For [remote editing](../remote.md), keep files and tools on another machine and
use a local display:

```sh
thc-edit --window buildbox:projects/example
```

## Documentation and development

The [guide index](../README.md) lists the user guides, and the
[F1 quick reference](../../README.md) collects everyday commands.
Read the [architecture](../architecture.md) and
[development guide](../contributing.md) to work on the editor itself.
The [documentation build guide](build.md) explains how this site is built and
published.
