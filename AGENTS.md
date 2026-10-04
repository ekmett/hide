# Working on hide

## Interaction performance

Never compare whole desktops, buffer contents, or undo histories to decide whether
an interaction or redraw needs work. Use explicit small UI-state keys and stable
identities or revisions for immutable payloads. Keep full-text processing on its
owning background worker. A new Desktop field must not silently add deep work to
the render loop; retain regression tests that reject forcing large payloads.

## Product direction

This is a greenfield project. Change interfaces when it improves the experience;
do not add aliases, version negotiation or migration machinery for hypothetical
legacy clients. Preserve user data and check stale actions, but prefer one clear
current interface. The visual direction is a modern editor loosely rooted in
Turbo Pascal, using our own renderer.
