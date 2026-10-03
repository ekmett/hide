# Working on hide

## Interaction performance

Never compare whole desktops, buffer contents, or undo histories to decide whether
an interaction or redraw needs work. Use explicit small UI-state keys and stable
identities or revisions for immutable payloads. Keep full-text processing on its
owning background worker. A new Desktop field must not silently add deep work to
the render loop; retain regression tests that reject forcing large payloads.
