# Editor guide

`thc-edit` is Turbo Haskell’s source editor. These guides cover editing files,
using Haskell language tools, building and debugging programs, reviewing Git
changes and working with agents. The same editor runs in a terminal, a native
window or a browser, locally or over SSH.

## Getting started

| Task | Guide |
| --- | --- |
| Build and launch the editor | [Installation](install.md) |
| Open files, navigate, search and arrange windows | [Editing](editing.md) |
| Detach, return later or change frontends | [Sessions](sessions.md) |
| Choose a frontend, change appearance or use the clipboard | [Display and frontends](display.md) |
| Set startup defaults and tool permissions | [Configuration](configuration.md) |
| Inspect and change binary files | [Hex editing](hex.md) |

[F1 Help](../README.md) is the short guide bundled with the editor. The command
`thc-edit --help` lists startup options.

## Working on a project

| Task | Guide |
| --- | --- |
| Inspect types, follow definitions, rename and handle diagnostics | [Haskell language tools](haskell.md) |
| Run a program, use a shell, attach a debugger | [Running and debugging](running.md) |
| Review, commit, fetch, pull and merge | [Git](git.md) |
| Give an agent access to the live editor and its tools | [Session tools](session-tools.md) |
| Choose a task workflow for the agent | [Agent skills](agent-skills.md) |
| Look up calls by category | [Agent operation reference](agent-tools.md) |
| Discuss code, provide context and review proposed changes | [Conversations](conversations.md) |
| Work on another machine with a local display | [Remote editing](remote.md) |

## Development and reference

[Development](contributing.md) gives the source layout and checks.
[Architecture](architecture.md) describes shared buffers, saves, rendering and
remote transport. The [design records](design/) and [implementation plans](plans/)
explain how the pieces were developed; use the guides above for the current
commands and workflow.
