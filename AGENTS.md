# Working on hide

## Interaction performance

Never compare whole desktops, buffer contents, or undo histories to decide whether
an interaction or redraw needs work. Use explicit small UI-state keys and stable
identities or revisions for immutable payloads. Keep full-text processing on its
owning background worker. A new Desktop field must not silently add deep work to
the render loop; retain regression tests that reject forcing large payloads.

Track heap allocation per actual rendered frame against reproducible, optimized
baselines at matching viewport, contents and interaction. Attribute costs to
scene construction, cell composition, transport and frontend upload; measure
retained CPU/GPU memory separately. Keep benchmark results private by default.

Investigate an increase above 2× the matching baseline. Growth into or beyond the
2–3× range needs a concrete feature benefit that clearly pays for the cost,
recorded with the evidence; do not silently reset a baseline to excuse a
regression. Reuse persistent glyph storage and frame staging buffers, respecting
in-flight GPU ownership. Keep focused allocation checks alongside the affected
renderer paths, and avoid building a second benchmark framework.

## Product direction

This is a greenfield project. Change interfaces when it improves the experience;
do not add aliases, version negotiation or migration machinery for hypothetical
legacy clients. Preserve user data and check stale actions, but prefer one clear
current interface. The visual direction is a modern editor loosely rooted in
Turbo Pascal, using our own renderer.
