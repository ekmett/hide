<p class="thc-eyebrow">Turbo Haskell editor</p>

# Source, types and running programs in one place

Open a project, follow a definition, change the code and review the result.
Keep source, diagnostics, conversations and running programs beside each other
in a terminal, a native window or the browser.

```sh
thc-edit .
```

[Install the editor](../install.md), then use the [editing guide](../editing.md)
for files, windows, search and shared split views. The [guide index](../README.md)
and [F1 quick reference](../../README.md) cover the everyday commands.

## Keep your place

A desktop lives in a session. Detach, return later, or resume it through another
frontend. [Sessions](../sessions.md) explains how to choose and continue an
unfinished desktop; [remote editing](../remote.md) keeps files and tools on
another machine while the display stays local.

```sh
thc-edit --resume
thc-edit --web --resume
thc-edit --window buildbox:projects/example
```

[Display and frontends](../display.md) covers appearance, clipboard support and
frontend controls. [Configuration](../configuration.md) covers startup defaults,
project context and tool permissions.

## Work from source through review

[Haskell language tools](../haskell.md) provide types, definitions, completion,
rename and diagnostics for live buffers. [Running and debugging](../running.md)
covers build targets, tests, shared terminals, breakpoints and stack inspection.
The [Git guide](../git.md) describes reviewing and committing saved changes.
For binary files, use the [hex editor](../hex.md).

## Bring an agent into the editor

[Conversations](../conversations.md) explains provider setup, context, permissions
and reviewing proposed changes. [Session tools](../session-tools.md) gives an agent
access to the live desktop, including unsaved edits, language tools and running
programs. Choose a workflow with [agent skills](../agent-skills.md), and find the
available calls in the [agent operation reference](../agent-tools.md).

## Understand and extend it

Read the [architecture](../architecture.md) and [development guide](../contributing.md)
for the editor's structure and checks. The [documentation build guide](build.md)
explains how this site is generated, checked and published.
