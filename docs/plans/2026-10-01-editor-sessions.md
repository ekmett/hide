# Editor sessions

An editor session owns the desktop, buffers, undo history, language tools,
conversations and terminals. Displays attach to that session. A display can go
away without destroying the work; File > Exit deliberately ends the session.

Use the existing private local endpoint and framed remote display protocol for
all frontends. Keep one controlling display per session. Supply a Vty protocol
client alongside the native and browser clients. Preserve native file-path
opening for local window drops; remote drops remain uploads.

`--resume [ID]` chooses the only unfinished session, or offers a numbered list.
An explicit ID or unique prefix selects one directly. Records carry the remote
host when needed; display choice belongs to the new attachment. Catchable
termination detaches, restores terminal state, drains accepted input briefly,
and prints the session ID. Forced termination cannot print, but the session
record allows discovery. The session process is not a reboot recovery journal.

ACP receives an editor MCP stdio server. It reads snapshots through the private
endpoint without claiming the display writer. Initial tools list windows and
buffers, read paged unsaved contents and read selections. They do not write.

Validation:

- Local and SSH detach/reconnect preserve content and exactly-once input.
- Browser → terminal → browser preserves dirty text, undo and clipboard state.
- SIGINT/SIGTERM and terminal Ctrl+] restore the display and print an ID.
- Exit closes the session and removes its discovery record.
- Multiple sessions can be listed/selected without silently choosing one.
- MCP sees unsaved Unicode buffers while display input continues.
- Basic builds retain terminal/session functionality without SDL or WebGL.
