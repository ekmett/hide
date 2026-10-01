# Conversations

Keep a conversation beside the source you are working on. Ask about selected
text or another open file, including unsaved work, then read the reply and review
proposed changes in the same desktop.

## Configure a provider

**Options > Agents** configures an ACP stdio provider. Enter its executable,
a JSON array of arguments and a JSON object of environment overrides. The
executable is launched directly; arguments are not interpreted by a shell.
Authenticate with the provider's own tools before starting it in the editor.

For Codex, install the published
[codex-acp adapter](https://github.com/agentclientprotocol/codex-acp) and enter:

| Field | Value |
| --- | --- |
| Executable | `codex-acp` |
| Arguments | `[]` |
| Environment | `{}` |

The adapter version exercised with the editor is
`@agentclientprotocol/codex-acp@2.0.1`. Its `CODEX_PATH` environment override can
select an existing Codex executable. Provider installation, authentication and
model access are separate from the editor.

Settings live in the user's `thc-edit` configuration directory. In a remote
session, configure the provider on the remote host, where it runs beside the
project.

## Send a query

Open **Tools > Conversation** and type in the draft at the bottom. Your messages
align right in cyan; replies align left in gray. Code keeps its syntax colors.

| Keys | Action |
| --- | --- |
| Enter | Send, or queue a query while a reply is active |
| Shift+Enter | Insert a newline |
| Ctrl+Enter | Steer the active reply, if the provider supports it |
| Escape | Cancel the active reply |
| Tab | Browse replies with the keyboard |

Typing while browsing replies returns to the draft. The draft grows with its
contents, then scrolls after twelve visible rows. Draft Undo and Redo remain
available while replies stream. Clickable status-bar actions offer the same
commands. Cancelling the active reply lets queued queries proceed in order.

Click the conversation title, or use **Tools > Conversation model**, to choose
from the provider's advertised models and reasoning settings. Choices are
disabled during a reply and take effect after confirmation. Providers without
these options keep a plain title. The lower-left frame shows reported context
usage, such as `37% · 148k/400k`, or `--` until those values are available.

## Work with live editor context

The editor automatically supplies its built-in MCP server to the ACP provider
when starting or resuming a conversation. There is no separate context picker
to configure. Ask the provider to look at a named file, the source selection or
the open windows; its tools use the editor's current state when called.

| Tool | Available context |
| --- | --- |
| `list_windows` | Window titles, numbers, geometry, active window and side panels |
| `list_buffers` | Open files and untitled buffers, paths and unsaved-change state |
| `read_buffer` | Current text, including unsaved edits, or bytes from a hex buffer |
| `read_selection` | Selected text and cursor offsets in a chosen window |

The guest can also use HLS, inspect diagnostics, arrange windows, navigate text
and hex buffers, preview/apply undo history, build/run/test, and work with shared
terminals. [Session tools](session-tools.md) describe the full interface, including
screenshots, checked diffs and source debugging. Mutating tools work on this live
session; buffer edits and saves are separate actions.

## Read and copy

Drag over reply text to select it. Copying inside one bubble gives plain text;
copying across bubbles adds `User:` and `Bot:` labels. Bubble borders and timestamp
separators are excluded. **Tools > Copy raw conversation** copies the original
Markdown instead. Pauses of five minutes or more get a local timestamp separator.

Replies use CommonMark, including highlighted fenced code, lists and tables.
Tables wrap to the available width and become labeled cells when columns no
longer fit. Tool calls appear as compact chevron rows. Click one to expand its
full request/update JSON; click again to collapse it.

A guest can ask a question inline with suggested choices and a free-text answer.
Choose an option or enter your own reply, then submit. Cancel dismisses the
question. Your unfinished conversation draft is retained separately.

## Review requested work

Permission dialogs show the provider's actual choices. **Review** opens a
read-only view of the request; **Tools > Conversation** returns to the pending
choice. Escape denies it.

ACP file-write requests ask for approval, preserve the old buffer in Undo
and use the normal checked save path. If the editor or disk version changed
while the request was pending, the stale write is rejected. These file requests
are bounded by the conversation's project directory.

The provider is a subprocess with its own tools and permissions. The editor's
file boundary does not sandbox that subprocess; configure those permissions in
the provider. With the optional terminal build, ACP terminal requests also ask
before running and use the editor's embedded terminals.

## Continue later

Detach the editor and use `thc-edit --resume` to return to the running desktop,
including its conversation. [Editor sessions](sessions.md) cover this workflow
across native windows, the browser, terminals and SSH.

**Tools > New session** starts a fresh conversation. **Tools > Resume session**
accepts a saved provider session ID; the latest ID is saved across editor restarts.
These menu actions manage the provider's conversation, independently of the
editor session selected by `--resume`.
Resumption uses capabilities advertised by the provider. Transcript replay
requires provider support for loading the conversation; a provider that only
resumes may continue the session without replaying its earlier messages.

See [Git](git.md) to review the resulting saved changes and [running](running.md)
to use the terminal alongside the conversation.
